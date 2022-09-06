#include <stdio.h>
#include <stdlib.h>

int main(void) {
  printf("return=%d\n", setenv(NULL, NULL, 0));
  return 0;
}
