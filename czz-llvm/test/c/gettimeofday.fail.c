#include <stdio.h>
#include <sys/time.h>
int main(int argc, char *argv[]) {
  struct timeval t;
  gettimeofday(&t, NULL);
  printf("tv_sec: %ld\ntv_usec: %ld", t.tv_sec, t.tv_usec);
  return 0;
}
