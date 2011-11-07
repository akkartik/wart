#undef char
#include<sys/socket.h>
#include<netdb.h>
#define char wchar_t

COMPILE_PRIM_FUNC(fork, primFunc_fork, L"()",
  return mkref(newNum(fork()));
)

COMPILE_PRIM_FUNC(wait_for_child, primFunc_wait_for_child, L"()",
  wait(NULL);
  return nil;
)

COMPILE_PRIM_FUNC(sleep, primFunc_sleep, L"($n)",
  sleep(toNum(lookup(L"$n")));
  return nil;
)

COMPILE_PRIM_FUNC(make-socket, primFunc_socket, L"($host $port)",
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  hostent *host = gethostbyname(&toAscii(toString(lookup(L"$host")).c_str())[0]);
  sockaddr_in s;  s.sin_family = AF_INET;
  bcopy((char*)host->h_addr, (char*)s.sin_addr.s_addr, host->h_length);
  s.sin_port = htons(toNum(lookup(L"$port")));
  connect(sockfd, (sockaddr*)&s, sizeof(s));
  return mkref(newNum(sockfd));
)

COMPILE_PRIM_FUNC(make-server-socket, primFunc_server_socket, L"($host $port)",
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  sockaddr_in s;  s.sin_family = AF_INET;   s.sin_addr.s_addr = INADDR_ANY;
  s.sin_port = htons(toNum(lookup(L"$port")));
  bind(sockfd, (sockaddr*)&s, sizeof(s));
  listen(sockfd, 5);
  return mkref(newNum(sockfd));
)

COMPILE_PRIM_FUNC(socket-accept, primFunc_socket_accept, L"($fd)",
  sockaddr_in s;  socklen_t n = sizeof(sockaddr_in);
  return mkref(newNum(accept(toNum(lookup(L"$fd")), (sockaddr*)&s, &n)));
)
