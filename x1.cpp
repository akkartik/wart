#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<unistd.h>
#include<sys/socket.h>
#include<netdb.h>

#define PERR(call...) if (call < 0) perror(#call)

char buf[BUFSIZ];
socklen_t n;
int foo1(int fd) {
  sockaddr_in t;  n = sizeof(sockaddr_in);
  return accept(fd, (sockaddr*)&t, &n);
}

int main() {
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) perror("socket() failed");
  int dummy;
  PERR(setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &dummy, sizeof(dummy)));
  sockaddr_in s;  s.sin_family = AF_INET;   s.sin_addr.s_addr = INADDR_ANY;
  s.sin_port = htons(4040L);
  PERR(bind(sockfd, (sockaddr*)&s, sizeof(s)));
  PERR(listen(sockfd, 5));

  int clientsockfd = foo1(sockfd);

  read(clientsockfd, buf, BUFSIZ-1);

  close(clientsockfd);
  close(sockfd);
  return 0;
}
