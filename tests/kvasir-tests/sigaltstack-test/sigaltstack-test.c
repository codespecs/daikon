/* A signal handler runs on an alternate signal stack that is a local
   array of main, so the alternate signal stack lies in the thread's
   stack segment, above the frames of X and Y.  Kvasir should treat it
   as a separate stack, and not pop the frames of X and Y when the
   handler is entered. */

#include <signal.h>
#include <stdio.h>
#include <string.h>

volatile int handled;

void H(int sig) {
  handled = sig;
}

int Y(int a) {
  raise(SIGUSR1);
  return a + 1;
}

int X(int a) {
  return Y(a) * 2;
}

int main(void) {
  char altstack[65536];
  stack_t ss;
  struct sigaction sa;
  int x;

  ss.ss_sp = altstack;
  ss.ss_size = sizeof altstack;
  ss.ss_flags = 0;
  if (sigaltstack(&ss, NULL) != 0) {
    perror("sigaltstack");
    return 1;
  }
  memset(&sa, 0, sizeof sa);
  sa.sa_handler = H;
  sa.sa_flags = SA_ONSTACK;
  sigaction(SIGUSR1, &sa, NULL);

  x = X(3);
  printf("%d %d\n", x, handled);
  return 0;
}
