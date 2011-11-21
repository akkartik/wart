#include<sys/socket.h>
#include<netdb.h>

COMPILE_PRIM_FUNC(system, primFunc_system, "($command)",
  return mkref(newNum(system(toString(lookup("$command")).c_str())));
)

COMPILE_PRIM_FUNC(fork, primFunc_fork, "()",
  return mkref(newNum(fork()));
)

COMPILE_PRIM_FUNC(wait_for_child, primFunc_wait_for_child, "()",
  wait();
  return nil;
)

COMPILE_PRIM_FUNC(sleep, primFunc_sleep, "($n)",
  sleep(toNum(lookup("$n")));
  return nil;
)

COMPILE_PRIM_FUNC(make-socket, primFunc_socket, "($host $port)",
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  hostent *host = gethostbyname(toString(lookup("$host")).c_str());
  sockaddr_in s;  s.sin_family = AF_INET;
  bcopy((char*)host->h_addr, (char*)s.sin_addr.s_addr, host->h_length);
  s.sin_port = htons(toNum(lookup("$port")));
  connect(sockfd, (sockaddr*)&s, sizeof(s));
  return mkref(newNum(sockfd));
)

COMPILE_PRIM_FUNC(make-server-socket, primFunc_server_socket, "($host $port)",
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  sockaddr_in s;  s.sin_family = AF_INET;   s.sin_addr.s_addr = INADDR_ANY;
  s.sin_port = htons(toNum(lookup("$port")));
  bind(sockfd, (sockaddr*)&s, sizeof(s));
  listen(sockfd, 5);
  return mkref(newNum(sockfd));
)

COMPILE_PRIM_FUNC(socket-accept, primFunc_socket_accept, "($fd)",
  sockaddr_in s;  socklen_t n = sizeof(sockaddr_in);
  return mkref(newNum(accept(toNum(lookup("$fd")), (sockaddr*)&s, &n)));
)
