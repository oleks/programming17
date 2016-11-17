#define _GNU_SOURCE
#define STACK_SIZE (0x10000)

#include <errno.h>
#include <error.h>
#include <sched.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

int worker0(void *arg) {
  arg = arg;
  execlp("true", "true", NULL);
  return 0;
}

int worker1(void *arg) {
  arg = arg;
  execlp("true", "true", NULL);
  return 0;
}

int main() {
  long retval0;
  char stack0[STACK_SIZE];
  retval0 = clone(worker0, stack0 + STACK_SIZE, SIGCHLD, NULL);
  if (retval0 < 0) {
    error(1, errno, "system b0rked ;-/");
  }

  long retval1;
  char stack1[STACK_SIZE];
  retval1 = clone(worker1, stack1 + STACK_SIZE, SIGCHLD, NULL);
  if (retval1 < 0) {
    error(1, errno, "system b0rked ;-/");
  }

  exit(EXIT_SUCCESS);
}
