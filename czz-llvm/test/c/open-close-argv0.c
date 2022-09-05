#include <fcntl.h>
#include <unistd.h>
int main(int argc, char *argv[]) {
  int fd = open(argv[0], O_RDWR);
  return close(fd);
}
