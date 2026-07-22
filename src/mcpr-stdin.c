// MCPR Lock-Free Stdin Reader
// Background native thread reads the stdin OS handle into a bounded line queue.
// Uses raw descriptor/handle I/O and never holds the C stdin FILE lock.

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <errno.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>
#endif

#define READ_BUF_SIZE (64 * 1024)
#define MAX_LINE_SIZE (16 * 1024 * 1024)
#define MAX_QUEUE_LINES 256
#define MAX_QUEUE_BYTES (64 * 1024 * 1024)

typedef struct line_node {
  char *data;
  size_t len;
  struct line_node *next;
} line_node;

static line_node *queue_head = NULL;
static line_node *queue_tail = NULL;
static size_t queue_lines = 0;
static size_t queue_bytes = 0;
static int started = 0;
static int eof_flag = 0;
static int stop_requested = 0;

#ifdef _WIN32
static CRITICAL_SECTION queue_mutex;
static CONDITION_VARIABLE queue_cond;
static INIT_ONCE sync_once = INIT_ONCE_STATIC_INIT;
static HANDLE reader_thread = NULL;

static BOOL CALLBACK init_sync(PINIT_ONCE once, PVOID param, PVOID *context) {
  (void) once;
  (void) param;
  (void) context;
  InitializeCriticalSection(&queue_mutex);
  InitializeConditionVariable(&queue_cond);
  return TRUE;
}

static void ensure_sync(void) {
  InitOnceExecuteOnce(&sync_once, init_sync, NULL, NULL);
}

static void queue_lock(void) { EnterCriticalSection(&queue_mutex); }
static void queue_unlock(void) { LeaveCriticalSection(&queue_mutex); }
static void queue_signal(void) { WakeConditionVariable(&queue_cond); }
static void queue_broadcast(void) { WakeAllConditionVariable(&queue_cond); }
static void queue_wait(int wait_ms) {
  SleepConditionVariableCS(&queue_cond, &queue_mutex, (DWORD) wait_ms);
}
#else
static pthread_mutex_t queue_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t queue_cond = PTHREAD_COND_INITIALIZER;
static pthread_t reader_thread;
static int reader_thread_created = 0;

static void ensure_sync(void) {}
static void queue_lock(void) { pthread_mutex_lock(&queue_mutex); }
static void queue_unlock(void) { pthread_mutex_unlock(&queue_mutex); }
static void queue_signal(void) { pthread_cond_signal(&queue_cond); }
static void queue_broadcast(void) { pthread_cond_broadcast(&queue_cond); }
static void queue_wait(int wait_ms) {
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  ts.tv_sec += wait_ms / 1000;
  ts.tv_nsec += (long) (wait_ms % 1000) * 1000000L;
  if (ts.tv_nsec >= 1000000000L) {
    ts.tv_sec += ts.tv_nsec / 1000000000L;
    ts.tv_nsec %= 1000000000L;
  }
  pthread_cond_timedwait(&queue_cond, &queue_mutex, &ts);
}
static void queue_unlock_cleanup(void *arg) {
  (void) arg;
  queue_unlock();
}
#endif

// Caller owns queue_mutex and transfers ownership of s. When the bounded
// queue is full, the reader waits and lets the OS pipe apply backpressure.
static void enqueue_line(char *s, size_t len) {
  while (!stop_requested &&
         (queue_lines >= MAX_QUEUE_LINES || queue_bytes + len > MAX_QUEUE_BYTES)) {
#ifdef _WIN32
    queue_wait(250);
#else
    pthread_cleanup_push(queue_unlock_cleanup, NULL);
    queue_wait(250);
    pthread_cleanup_pop(0);
#endif
  }
  if (stop_requested) {
    free(s);
    return;
  }
  line_node *node = (line_node *) malloc(sizeof(line_node));
  if (node == NULL) {
    free(s);
    return;
  }
  node->data = s;
  node->len = len;
  node->next = NULL;
  if (queue_tail == NULL) {
    queue_head = node;
    queue_tail = node;
  } else {
    queue_tail->next = node;
    queue_tail = node;
  }
  queue_lines++;
  queue_bytes += len;
}

static void emit_line(const char *acc, size_t len) {
  if (len > 0 && acc[len - 1] == '\r') len--;
  char *s = (char *) malloc(len + 1);
  if (s == NULL) return;
  memcpy(s, acc, len);
  s[len] = '\0';

  queue_lock();
  enqueue_line(s, len);
  queue_signal();
  queue_unlock();
}

static void set_eof(void) {
  queue_lock();
  eof_flag = 1;
  queue_signal();
  queue_unlock();
}

#ifndef _WIN32
static void free_accumulator(void *ptr) {
  char **accumulator = (char **) ptr;
  free(*accumulator);
}
#endif

static void reader_loop(void) {
  char buf[READ_BUF_SIZE];
  size_t acc_cap = 4096;
  size_t acc_len = 0;
  char *acc = (char *) malloc(acc_cap);
  int oversize = 0;

  if (acc == NULL) {
    set_eof();
    return;
  }

#ifndef _WIN32
  pthread_cleanup_push(free_accumulator, &acc);
#endif

  for (;;) {
    queue_lock();
    int should_stop = stop_requested;
    queue_unlock();
    if (should_stop) break;
#ifdef _WIN32
    DWORD n = 0;
    BOOL ok = ReadFile(GetStdHandle(STD_INPUT_HANDLE), buf, sizeof(buf), &n, NULL);
    if (!ok || n == 0) break;
    size_t count = (size_t) n;
#else
    ssize_t n = read(STDIN_FILENO, buf, sizeof(buf));
    if (n < 0) {
      if (errno == EINTR) continue;
      break;
    }
    if (n == 0) break;
    size_t count = (size_t) n;
#endif

    for (size_t i = 0; i < count; i++) {
      char c = buf[i];
      if (c == '\n') {
        if (!oversize) emit_line(acc, acc_len);
        acc_len = 0;
        oversize = 0;
        continue;
      }
      if (oversize) continue;
      if (acc_len + 1 > MAX_LINE_SIZE) {
        oversize = 1;
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

  if (!oversize && acc_len > 0) emit_line(acc, acc_len);
#ifdef _WIN32
  free(acc);
#else
  pthread_cleanup_pop(1);
#endif
  set_eof();
}

#ifdef _WIN32
static DWORD WINAPI reader_main(LPVOID arg) {
  (void) arg;
  reader_loop();
  return 0;
}
#else
static void *reader_main(void *arg) {
  (void) arg;
  reader_loop();
  return NULL;
}
#endif

SEXP mcpr_stdin_start(void) {
  ensure_sync();
  queue_lock();
  if (started) {
    queue_unlock();
    return Rf_ScalarLogical(TRUE);
  }
  eof_flag = 0;
  stop_requested = 0;
  started = 1;
  queue_unlock();

#ifdef _WIN32
  reader_thread = CreateThread(NULL, 0, reader_main, NULL, 0, NULL);
  if (reader_thread == NULL) {
    queue_lock();
    started = 0;
    queue_unlock();
    return Rf_ScalarLogical(FALSE);
  }
#else
  if (pthread_create(&reader_thread, NULL, reader_main, NULL) != 0) {
    queue_lock();
    started = 0;
    queue_unlock();
    return Rf_ScalarLogical(FALSE);
  }
  reader_thread_created = 1;
#endif

  return Rf_ScalarLogical(TRUE);
}

SEXP mcpr_stdin_poll(SEXP timeout_ms) {
  int wait_ms = Rf_asInteger(timeout_ms);
  ensure_sync();
  queue_lock();
  if (queue_head == NULL && !eof_flag && wait_ms > 0) queue_wait(wait_ms);

  if (queue_head != NULL) {
    line_node *node = queue_head;
    queue_head = node->next;
    if (queue_head == NULL) queue_tail = NULL;
    queue_lines--;
    queue_bytes -= node->len;
    queue_signal();
    queue_unlock();
    SEXP out = PROTECT(Rf_mkString(node->data));
    free(node->data);
    free(node);
    UNPROTECT(1);
    return out;
  }
  if (eof_flag) {
    queue_unlock();
    return Rf_ScalarLogical(FALSE);
  }
  queue_unlock();
  return R_NilValue;
}

SEXP mcpr_stdin_stop(void) {
  ensure_sync();
  queue_lock();
  stop_requested = 1;
  queue_broadcast();
  queue_unlock();
#ifdef _WIN32
  if (reader_thread != NULL) {
    CancelSynchronousIo(reader_thread);
    DWORD wait_result = WaitForSingleObject(reader_thread, 2000);
    if (wait_result != WAIT_OBJECT_0) return Rf_ScalarLogical(FALSE);
    CloseHandle(reader_thread);
    reader_thread = NULL;
  }
#else
  if (reader_thread_created) {
    pthread_cancel(reader_thread);
    pthread_join(reader_thread, NULL);
    reader_thread_created = 0;
  }
#endif

  queue_lock();
  line_node *node = queue_head;
  while (node != NULL) {
    line_node *next = node->next;
    free(node->data);
    free(node);
    node = next;
  }
  queue_head = NULL;
  queue_tail = NULL;
  queue_lines = 0;
  queue_bytes = 0;
  eof_flag = 0;
  started = 0;
  stop_requested = 0;
  queue_broadcast();
  queue_unlock();
  return Rf_ScalarLogical(TRUE);
}
