//// bindings for some OS services

COMPILE_FN(system, compiledFn_system, "($command)",
  return mkref(newNum(system(toString(lookup("$command")).c_str())));
)

COMPILE_FN(fork, compiledFn_fork, "()",
  return mkref(newNum(fork()));
)

#include<sys/wait.h>

COMPILE_FN(wait_for_child, compiledFn_wait_for_child, "()",
  wait(NULL);
  return nil;
)

COMPILE_FN(sleep, compiledFn_sleep, "($n)",
  sleep(toInt(lookup("$n")));
  return nil;
)

#include<sys/socket.h>
#include<netdb.h>

#define PERR(call...) if (call < 0) perror(#call)

struct Socket {
  int fd;
  sockaddr_in addr;
};

Cell* newSocket(Socket* s) {
  return newObject("socket", newNum((long)s));
}

Socket* toSocket(Cell* s) {
  if (type(s) != newSym("socket"))
    RAISE << "not a socket: " << s << endl << DIE;
  return (Socket*)toInt(car(cdr(cdr(s))));
}

COMPILE_FN(socket_fd, compiledFn_socket_fd, "($sock)",
  return mkref(newNum(toSocket(lookup("$sock"))->fd));
)

COMPILE_FN(make_socket, compiledFn_socket, "($host $port)",
  Socket* sock = new Socket();
  sock->fd = socket(AF_INET, SOCK_STREAM, 0);
  if (sock->fd < 0) perror("socket() failed");
  hostent *host = gethostbyname(toString(lookup("$host")).c_str());
  bzero(&sock->addr, sizeof(sock->addr));
  sock->addr.sin_family = AF_INET;
  bcopy((char*)&host->h_addr, (char*)&sock->addr.sin_addr.s_addr, host->h_length);
  sock->addr.sin_port = htons(toInt(lookup("$port")));
  PERR(connect(sock->fd, (sockaddr*)&sock->addr, sizeof(sock->addr)));
  return mkref(newSocket(sock));
)

COMPILE_FN(make_server_socket, compiledFn_server_socket, "($port)",
  Socket* sock = new Socket();
  sock->fd = socket(AF_INET, SOCK_STREAM, 0);
  if (sock->fd < 0) perror("sock() failed");
  int dummy;
  PERR(setsockopt(sock->fd, SOL_SOCKET, SO_REUSEADDR, &dummy, sizeof(dummy)));
  bzero(&sock->addr, sizeof(sock->addr));
  sock->addr.sin_family = AF_INET;   sock->addr.sin_addr.s_addr = INADDR_ANY;
  sock->addr.sin_port = htons(toInt(lookup("$port")));
  PERR(bind(sock->fd, (sockaddr*)&sock->addr, sizeof(sock->addr)));
  PERR(listen(sock->fd, 5));
  return mkref(newSocket(sock));
)

COMPILE_FN(socket_accept, compiledFn_socket_accept, "($sock)",
  Socket* sock = toSocket(lookup("$sock"));
  Socket* clientsock = new Socket();  socklen_t n = sizeof(clientsock->addr);
  bzero(&clientsock->addr, n);
  clientsock->fd = accept(sock->fd, (sockaddr*)&clientsock->addr, &n);
  if (clientsock->fd < 0) perror("bind");
  return mkref(newSocket(clientsock));
)

COMPILE_FN(close_socket, compiledFn_close_socket, "($sock)",
  Socket* sock = toSocket(lookup("$sock"));
  close(sock->fd);
  delete sock;
  setCar(cdr(cdr(lookup("$sock"))), nil);
  return nil;
)



#include<signal.h>

struct sigaction originalSignalHandler;
void interrupt(int s) {
  Cell* f = lookupDynamicBinding(newSym("on_interrupt"));
  if (f)
    evalUnbox(newCons(f));  // leak
  if (originalSignalHandler.sa_handler)
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
