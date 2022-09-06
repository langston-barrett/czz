#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  int success = unsetenv("FOO");
  printf("success=%d\n", success);
  printf("FOO=%d\n", getenv("FOO") == NULL);
  return 0;
}
