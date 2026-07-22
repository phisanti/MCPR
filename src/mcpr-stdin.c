// MCPR Lock-Free Stdin Reader
// Background pthread reads STDIN_FILENO via raw read(2) into a mutex/condvar line queue.
// Never touches the stdin FILE lock (no fgetc/flockfile), so it cannot deadlock R's main thread.

#include <R.h>
#include <Rinternals.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#define READ_BUF_SIZE (64 * 1024)
#define MAX_LINE_SIZE (16 * 1024 * 1024)

typedef struct line_node {
  char *data;
  struct line_node *next;
} line_node;

static pthread_mutex_t queue_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  queue_cond  = PTHREAD_COND_INITIALIZER;
static pthread_t       reader_thread;

static line_node *queue_head = NULL;
static line_node *queue_tail = NULL;

static int started = 0;
static int stopped = 0;
static int eof_flag = 0;

// Enqueue a NUL-terminated line (ownership of `s` transferred to the queue).
// Caller must hold queue_mutex.
static void enqueue_line(char *s) {
  line_node *node = (line_node *) malloc(sizeof(line_node));
  if (node == NULL) {
    free(s);
    return;
  }
  node->data = s;
  node->next = NULL;
  if (queue_tail == NULL) {
    queue_head = node;
    queue_tail = node;
  } else {
    queue_tail->next = node;
    queue_tail = node;
  }
}

// Emit the accumulated bytes as one line: strip a trailing '\r', NUL-terminate,
// enqueue, and signal. Oversize accumulators are dropped by the caller before here.
static void emit_line(const char *acc, size_t len) {
  if (len > 0 && acc[len - 1] == '\r') {
    len--;
  }
  char *s = (char *) malloc(len + 1);
  if (s == NULL) {
    return;
  }
  memcpy(s, acc, len);
  s[len] = '\0';

  pthread_mutex_lock(&queue_mutex);
  enqueue_line(s);
  pthread_cond_signal(&queue_cond);
  pthread_mutex_unlock(&queue_mutex);
}

static void set_eof(void) {
  pthread_mutex_lock(&queue_mutex);
  eof_flag = 1;
  pthread_cond_signal(&queue_cond);
  pthread_mutex_unlock(&queue_mutex);
}

static void *reader_main(void *arg) {
  char buf[READ_BUF_SIZE];

  // Growable accumulator for a single in-progress line.
  size_t acc_cap = 4096;
  size_t acc_len = 0;
  char *acc = (char *) malloc(acc_cap);
  int oversize = 0;  // current line exceeded MAX_LINE_SIZE; drop until next '\n'

  if (acc == NULL) {
    set_eof();
    return NULL;
  }

  for (;;) {
    ssize_t n = read(STDIN_FILENO, buf, sizeof buf);

    if (n < 0) {
      if (errno == EINTR) continue;
      break;  // any other read error: treat as EOF
    }
    if (n == 0) {
      // EOF: flush any buffered trailing partial line, then signal EOF.
      if (!oversize && acc_len > 0) {
        emit_line(acc, acc_len);
      }
      break;
    }

    for (ssize_t i = 0; i < n; i++) {
      char c = buf[i];
      if (c == '\n') {
        if (!oversize) {
          emit_line(acc, acc_len);
        }
        acc_len = 0;
        oversize = 0;
        continue;
      }
      if (oversize) continue;  // still discarding an over-long line
      if (acc_len + 1 > MAX_LINE_SIZE) {
        oversize = 1;  // drop this line entirely; keep scanning for '\n'
        acc_len = 0;
        continue;
      }
      if (acc_len + 1 > acc_cap) {
        size_t new_cap = acc_cap * 2;
        if (new_cap > MAX_LINE_SIZE + 1) new_cap = MAX_LINE_SIZE + 1;
        char *tmp = (char *) realloc(acc, new_cap);
        if (tmp == NULL) {
          oversize = 1;
          acc_len = 0;
          continue;
        }
        acc = tmp;
        acc_cap = new_cap;
      }
      acc[acc_len++] = c;
    }
  }

  free(acc);
  set_eof();
  return NULL;
}

SEXP mcpr_stdin_start(void) {
  pthread_mutex_lock(&queue_mutex);
  if (started) {
    pthread_mutex_unlock(&queue_mutex);
    return R_NilValue;
  }
  started = 1;
  pthread_mutex_unlock(&queue_mutex);

  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  pthread_create(&reader_thread, &attr, reader_main, NULL);
  pthread_attr_destroy(&attr);

  return R_NilValue;
}

SEXP mcpr_stdin_poll(SEXP timeout_ms) {
  int wait_ms = Rf_asInteger(timeout_ms);

  pthread_mutex_lock(&queue_mutex);

  if (queue_head == NULL && !eof_flag && wait_ms != 0) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec  += wait_ms / 1000;
    ts.tv_nsec += (long)(wait_ms % 1000) * 1000000L;
    if (ts.tv_nsec >= 1000000000L) {
      ts.tv_sec  += ts.tv_nsec / 1000000000L;
      ts.tv_nsec %= 1000000000L;
    }
    pthread_cond_timedwait(&queue_cond, &queue_mutex, &ts);
  }

  if (queue_head != NULL) {
    line_node *node = queue_head;
    queue_head = node->next;
    if (queue_head == NULL) queue_tail = NULL;
    pthread_mutex_unlock(&queue_mutex);

    SEXP out = Rf_mkString(node->data);
    free(node->data);
    free(node);
    return out;
  }

  if (eof_flag) {
    pthread_mutex_unlock(&queue_mutex);
    return Rf_ScalarLogical(FALSE);  // EOF sentinel
  }

  pthread_mutex_unlock(&queue_mutex);
  return R_NilValue;  // timeout, nothing available
}

SEXP mcpr_stdin_stop(void) {
  pthread_mutex_lock(&queue_mutex);
  stopped = 1;
  line_node *node = queue_head;
  while (node != NULL) {
    line_node *next = node->next;
    free(node->data);
    free(node);
    node = next;
  }
  queue_head = NULL;
  queue_tail = NULL;
  pthread_cond_signal(&queue_cond);
  pthread_mutex_unlock(&queue_mutex);
  // The detached reader thread may remain parked in read(); that is acceptable
  // since mcpr_stdin_stop runs only on process shutdown. Do not pthread_join.
  return R_NilValue;
}
