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
  execlp("ls", "ls", "/", "/home/", NULL);
  return 0;
}

int main() {
  long retval0;
  char stack0[STACK_SIZE];
  retval0 = clone(worker0, stack0 + STACK_SIZE, SIGCHLD, NULL);
  if (retval0 < 0) {
    error(1, errno, "system b0rked ;-/");
  }

  exit(EXIT_SUCCESS);
}
