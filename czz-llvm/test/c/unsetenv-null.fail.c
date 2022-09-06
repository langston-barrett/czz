#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  printf("return=%d\n", unsetenv(NULL));
  return 0;
}
