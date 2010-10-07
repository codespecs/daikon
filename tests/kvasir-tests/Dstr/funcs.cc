#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>

// XTide uses side-effects from asserts.  Disable "optimization" that
// eliminates these.
#undef NDEBUG
#include <assert.h>

#include "Dstr.hh"

#ifndef max
#define max(a,b) (a < b ? b : a)
#endif
#ifndef min
#define min(a,b) (a > b ? b : a)
#endif

static int daemon_mode = 0;

void log (const char *message, int priority) {
  if (message) {
    fprintf (stderr, "%s (%d)\n", message, daemon_mode);
  }
}

void log (const char *message, Dstr &details, int priority) {
  Dstr temp (message);
  temp += details;
  log (temp.aschar(), priority);
}
