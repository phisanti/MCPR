// MCPR Native Routine Registration
// Registers the compiled .Call entry points for the lock-free stdin reader.
// Uses R_registerRoutines + R_useDynamicSymbols(FALSE) for symbol-based dispatch.

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP mcpr_stdin_start(void);
extern SEXP mcpr_stdin_poll(SEXP timeout_ms);
extern SEXP mcpr_stdin_stop(void);
extern SEXP mcpr_watchdog_start(SEXP launcher_pid, SEXP poll_ms);
extern SEXP mcpr_watchdog_stop(void);

static const R_CallMethodDef CallEntries[] = {
  {"mcpr_stdin_start",    (DL_FUNC) &mcpr_stdin_start,    0},
  {"mcpr_stdin_poll",     (DL_FUNC) &mcpr_stdin_poll,     1},
  {"mcpr_stdin_stop",     (DL_FUNC) &mcpr_stdin_stop,     0},
  {"mcpr_watchdog_start", (DL_FUNC) &mcpr_watchdog_start, 2},
  {"mcpr_watchdog_stop",  (DL_FUNC) &mcpr_watchdog_stop,  0},
  {NULL, NULL, 0}
};

void R_init_MCPR(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
