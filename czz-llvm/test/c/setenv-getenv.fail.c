#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  int success = setenv("FOO", "bar", 0);
  printf("success=%d\n", success);
  printf("FOO=%s\n", getenv("FOO"));
  return 0;
}
