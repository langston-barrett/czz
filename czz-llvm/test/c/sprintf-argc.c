#include <stdio.h>
int main(int argc, char *argv[]) {
  char buf[8];
  sprintf(buf, "%d", argc);
  printf("%s\n", buf);
  return 0;
}
