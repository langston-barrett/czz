#include <stdio.h>
int main(int argc, char *argv[]) {
  FILE *f = fopen(argv[0], "r");
  return fclose(f);
}
