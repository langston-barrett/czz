#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  printf("return=%d\n", unsetenv(NULL));
  printf("errno=%d\n", errno);
  return 0;
}
