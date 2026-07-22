// MCPR Parent-Death Watchdog
// Background pthread that force-exits the process when it is orphaned, so a broker
// wedged in native user code (e.g. while(TRUE)NULL) still dies when its launcher does.
// Backstop for the in-loop is_orphaned() check, which cannot run while R is blocked.

#include <R.h>
#include <Rinternals.h>
#include <pthread.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <errno.h>
#include <time.h>

static pthread_t wd_thread;
static int       wd_started = 0;
static pid_t     wd_launcher_pid = 0;
static long      wd_poll_ms = 250;

// Poll parent liveness; _exit as soon as the process is orphaned. Runs entirely
// off the R main thread, so it fires even when the interpreter is stuck in a
// non-yielding evaluation. Fires only on genuine orphaning, never in steady state.
static void *wd_main(void *arg) {
  (void) arg;
  struct timespec ts;
  ts.tv_sec  = wd_poll_ms / 1000;
  ts.tv_nsec = (wd_poll_ms % 1000) * 1000000L;
  for (;;) {
    // Reparented to init (parent died and we were adopted by PID 1).
    if (getppid() <= 1) {
      _exit(0);
    }
    // Recorded launcher no longer exists (kill(pid, 0) fails with ESRCH).
    if (wd_launcher_pid > 1 && kill(wd_launcher_pid, 0) != 0 && errno == ESRCH) {
      _exit(0);
    }
    nanosleep(&ts, NULL);
  }
  return NULL;
}

// Start the watchdog. launcher_pid is the PID recorded at server startup; poll_ms
// bounds detection latency. Idempotent: a second call is a no-op.
SEXP mcpr_watchdog_start(SEXP launcher_pid, SEXP poll_ms) {
  if (wd_started) {
    return R_NilValue;
  }
  wd_started = 1;
  wd_launcher_pid = (pid_t) Rf_asInteger(launcher_pid);
  int ms = Rf_asInteger(poll_ms);
  if (ms > 0) {
    wd_poll_ms = ms;
  }

  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  pthread_create(&wd_thread, &attr, wd_main, NULL);
  pthread_attr_destroy(&attr);

  return R_NilValue;
}
