#include <stdio.h>
int main(int argc, char *argv[]) {
  char buf[64];
  int ret = snprintf(buf, 1, "%d", argc);
  printf("%d %s\n", ret, buf);
  return 0;
}
