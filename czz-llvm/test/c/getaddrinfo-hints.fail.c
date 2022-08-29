#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>

#define BUF 256

int main(void) {
  const char *host = "localhost";
  struct addrinfo hints, *result;
  char addrstr[BUF];
  void *ptr;

  memset(&hints, 0, sizeof(hints));
  hints.ai_family = PF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags |= AI_CANONNAME;

  int err = getaddrinfo(host, NULL, &hints, &result);
  if (err != 0) {
    return err;
  }

  while (result) {
    inet_ntop(result->ai_family, result->ai_addr->sa_data, addrstr, BUF);

    switch (result->ai_family) {
    case AF_INET:
      ptr = &((struct sockaddr_in *)result->ai_addr)->sin_addr;
      break;
    case AF_INET6:
      ptr = &((struct sockaddr_in6 *)result->ai_addr)->sin6_addr;
      break;
    }
    inet_ntop(result->ai_family, ptr, addrstr, BUF);
    printf("IPv%d addresults: %s (%s)\n", result->ai_family == PF_INET6 ? 6 : 4,
           addrstr, result->ai_canonname);
    result = result->ai_next;
  }

  freeaddrinfo(result);
  return 0;
}
