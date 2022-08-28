#include <stdio.h>
int main(int argc, char *argv[]) {
  char buf[64];
  int ret = snprintf(buf, 0, "%d", argc);
  printf("%d %s\n", ret, buf);
  return 0;
}
