#include <stdio.h>
#include <time.h>
int main(int argc, char *argv[]) {
  time_t t = time(NULL);
  printf("time: %lu\n", t);
  return 0;
}
