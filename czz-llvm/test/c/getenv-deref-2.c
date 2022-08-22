#include <stdlib.h>
int main(void) {
  if (getenv("FOO") != 0) {
    return getenv("BAR")[0];
  }
}
