#include <signal.h>
#include <stdio.h>
void ignore_sigint(int sig) { return; }
int main(int argc, char *argv[]) {
  __sighandler_t prev = signal(SIGINT, ignore_sigint);
  printf("%d\n", prev == NULL);
  return 0;
}
