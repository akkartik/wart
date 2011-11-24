COMPILE_PRIM_FUNC(system, primFunc_system, "($command)",
  return mkref(newNum(system(toString(lookup("$command")).c_str())));
)

COMPILE_PRIM_FUNC(fork, primFunc_fork, "()",
  return mkref(newNum(fork()));
)

#include<sys/wait.h>

COMPILE_PRIM_FUNC(wait_for_child, primFunc_wait_for_child, "()",
  wait(NULL);
  return nil;
)

COMPILE_PRIM_FUNC(sleep, primFunc_sleep, "($n)",
  sleep(toNum(lookup("$n")));
  return nil;
)

#include<sys/socket.h>
#include<netdb.h>

#define PERR(call...) if (call < 0) perror(#call)

COMPILE_PRIM_FUNC(make-socket, primFunc_socket, "($host $port)",
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) perror("socket() failed");
  hostent *host = gethostbyname(toString(lookup("$host")).c_str());
  sockaddr_in s;  s.sin_family = AF_INET;
  bcopy((char*)host->h_addr, (char*)s.sin_addr.s_addr, host->h_length);
  s.sin_port = htons(toNum(lookup("$port")));
  PERR(connect(sockfd, (sockaddr*)&s, sizeof(s)));
  return mkref(newNum(sockfd));
)

COMPILE_PRIM_FUNC(make-server-socket, primFunc_server_socket, "($port)",
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) perror("socket() failed");
  int dummy;
  PERR(setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &dummy, sizeof(dummy)));
  sockaddr_in s;  s.sin_family = AF_INET;   s.sin_addr.s_addr = INADDR_ANY;
  s.sin_port = htons(toNum(lookup("$port")));
  PERR(bind(sockfd, (sockaddr*)&s, sizeof(s)));
  PERR(listen(sockfd, 5));
  return mkref(newNum(sockfd));
)

COMPILE_PRIM_FUNC(socket-accept, primFunc_socket_accept, "($fd)",
  sockaddr_in s;  socklen_t n = sizeof(sockaddr_in);
  return mkref(newNum(accept(toNum(lookup("$fd")), (sockaddr*)&s, &n)));
)

COMPILE_PRIM_FUNC(close, primFunc_close, "($fd)",
  close(toNum(lookup("$fd")));
  return nil;
)

COMPILE_PRIM_FUNC(readfoo, primFunc_readc, "($infd)",
  char buf[BUFSIZ];
  read(toNum(lookup("$infd")), buf, BUFSIZ-1);
  return mkref(newString(buf));
)

COMPILE_PRIM_FUNC(serverc, primFunc_foo, "($port)",
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) perror("socket() failed");
  int dummy;
  PERR(setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &dummy, sizeof(dummy)));
  sockaddr_in s;  s.sin_family = AF_INET;   s.sin_addr.s_addr = INADDR_ANY;
  s.sin_port = htons(toNum(lookup("$port")));
  PERR(bind(sockfd, (sockaddr*)&s, sizeof(s)));
  PERR(listen(sockfd, 5));

  sockaddr_in t;  socklen_t n = sizeof(sockaddr_in);
  int clientsockfd = accept(sockfd, (sockaddr*)&t, &n);

  char buf[BUFSIZ];
  read(clientsockfd, buf, BUFSIZ-1);

  close(sockfd);
  return nil;
)

COMPILE_PRIM_FUNC(serverc2, primFunc_serverc2, "($port)",
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) perror("socket() failed");
  int dummy;
  PERR(setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &dummy, sizeof(dummy)));
  sockaddr_in s;  s.sin_family = AF_INET;   s.sin_addr.s_addr = INADDR_ANY;
  s.sin_port = htons(toNum(lookup("$port")));
  PERR(bind(sockfd, (sockaddr*)&s, sizeof(s)));

  PERR(listen(sockfd, 5));
  sockaddr_in t;  socklen_t n = sizeof(sockaddr_in);
  int clientsockfd = accept(sockfd, (sockaddr*)&t, &n);
  char buf[BUFSIZ];
  read(clientsockfd, buf, BUFSIZ-1);

  close(sockfd);
  return nil;
)



#include<signal.h>

struct sigaction originalSignalHandler;
void interrupt(int s) {
  eval(newCons(lookup("on-interrupt"), nil));
  originalSignalHandler.sa_handler(s);
}

void catchCtrlC() {
  struct sigaction curr;
  curr.sa_handler = interrupt;
  sigemptyset(&curr.sa_mask);
  curr.sa_flags = 0;
  sigaction(SIGINT, NULL, &originalSignalHandler);
  sigaction(SIGINT, &curr, NULL);
}
