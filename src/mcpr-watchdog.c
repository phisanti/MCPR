// MCPR Parent-Death Watchdog
// Independent native thread terminates the server after its launcher exits.
// Runs outside the R evaluator and has an explicit normal-shutdown path.

#include <R.h>
#include <Rinternals.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#endif

static int wd_started = 0;

#ifdef _WIN32
static HANDLE wd_thread = NULL;
static HANDLE wd_parent = NULL;
static HANDLE wd_stop_event = NULL;

static DWORD WINAPI wd_main(LPVOID arg) {
  (void) arg;
  HANDLE handles[2] = {wd_parent, wd_stop_event};
  DWORD result = WaitForMultipleObjects(2, handles, FALSE, INFINITE);
  if (result == WAIT_OBJECT_0) {
    TerminateProcess(GetCurrentProcess(), 0);
  }
  return 0;
}
#else
static pthread_t wd_thread;
static int wd_thread_created = 0;
static pthread_mutex_t wd_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t wd_cond = PTHREAD_COND_INITIALIZER;
static int wd_stop = 0;
static pid_t wd_launcher_pid = 0;
static long wd_poll_ms = 250;

static void *wd_main(void *arg) {
  (void) arg;
  struct timespec ts;
  ts.tv_sec = wd_poll_ms / 1000;
  ts.tv_nsec = (wd_poll_ms % 1000) * 1000000L;
  for (;;) {
    pthread_mutex_lock(&wd_mutex);
    int should_stop = wd_stop;
    pthread_mutex_unlock(&wd_mutex);
    if (should_stop) return NULL;
    if (getppid() <= 1 ||
        (wd_launcher_pid > 1 && kill(wd_launcher_pid, 0) != 0 && errno == ESRCH)) {
      kill(getpid(), SIGKILL);
      return NULL;
    }
    struct timespec deadline;
    clock_gettime(CLOCK_REALTIME, &deadline);
    deadline.tv_sec += ts.tv_sec;
    deadline.tv_nsec += ts.tv_nsec;
    if (deadline.tv_nsec >= 1000000000L) {
      deadline.tv_sec++;
      deadline.tv_nsec -= 1000000000L;
    }
    pthread_mutex_lock(&wd_mutex);
    if (!wd_stop) pthread_cond_timedwait(&wd_cond, &wd_mutex, &deadline);
    pthread_mutex_unlock(&wd_mutex);
  }
  return NULL;
}
#endif

SEXP mcpr_watchdog_start(SEXP launcher_pid, SEXP poll_ms) {
  if (wd_started) return Rf_ScalarLogical(TRUE);
  int pid = Rf_asInteger(launcher_pid);

#ifdef _WIN32
  (void) poll_ms;
  if (pid <= 0) return Rf_ScalarLogical(FALSE);
  wd_parent = OpenProcess(SYNCHRONIZE, FALSE, (DWORD) pid);
  wd_stop_event = CreateEvent(NULL, TRUE, FALSE, NULL);
  if (wd_parent == NULL || wd_stop_event == NULL) {
    if (wd_parent != NULL) CloseHandle(wd_parent);
    if (wd_stop_event != NULL) CloseHandle(wd_stop_event);
    wd_parent = NULL;
    wd_stop_event = NULL;
    return Rf_ScalarLogical(FALSE);
  }
  wd_thread = CreateThread(NULL, 0, wd_main, NULL, 0, NULL);
  if (wd_thread == NULL) {
    CloseHandle(wd_parent);
    CloseHandle(wd_stop_event);
    wd_parent = NULL;
    wd_stop_event = NULL;
    return Rf_ScalarLogical(FALSE);
  }
#else
  wd_launcher_pid = (pid_t) pid;
  int ms = Rf_asInteger(poll_ms);
  if (ms > 0) wd_poll_ms = ms;
  pthread_mutex_lock(&wd_mutex);
  wd_stop = 0;
  pthread_mutex_unlock(&wd_mutex);
  if (pthread_create(&wd_thread, NULL, wd_main, NULL) != 0) {
    return Rf_ScalarLogical(FALSE);
  }
  wd_thread_created = 1;
#endif

  wd_started = 1;
  return Rf_ScalarLogical(TRUE);
}

SEXP mcpr_watchdog_stop(void) {
  if (!wd_started) return R_NilValue;
#ifdef _WIN32
  SetEvent(wd_stop_event);
  DWORD wait_result = WaitForSingleObject(wd_thread, 2000);
  if (wait_result != WAIT_OBJECT_0) return Rf_ScalarLogical(FALSE);
  CloseHandle(wd_thread);
  CloseHandle(wd_parent);
  CloseHandle(wd_stop_event);
  wd_thread = NULL;
  wd_parent = NULL;
  wd_stop_event = NULL;
#else
  pthread_mutex_lock(&wd_mutex);
  wd_stop = 1;
  pthread_cond_signal(&wd_cond);
  pthread_mutex_unlock(&wd_mutex);
  if (wd_thread_created) {
    pthread_join(wd_thread, NULL);
    wd_thread_created = 0;
  }
#endif
  wd_started = 0;
  return Rf_ScalarLogical(TRUE);
}
