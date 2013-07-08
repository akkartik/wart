//// bindings for some OS services

COMPILE_FN(system, compiledfn_system, "($command)",
  return mkref(new_num(system(to_string(lookup("$command")).c_str())));
)

COMPILE_FN(fork, compiledfn_fork, "()",
  return mkref(new_num(fork()));
)

#include<sys/wait.h>

COMPILE_FN(wait_for_child, compiledfn_wait_for_child, "()",
  wait(NULL);
  return nil;
)

COMPILE_FN(sleep, compiledfn_sleep, "($n)",
  sleep(to_int(lookup("$n")));
  return nil;
)

#include<sys/socket.h>
#include<netdb.h>

#define PERR(call...)  if (call < 0) perror(#call)

struct socket_type {
  int fd;
  sockaddr_in addr;
};

cell* new_socket(socket_type* s) {
  return new_object("socket", new_num((long)s));
}

socket_type* to_socket(cell* s) {
  if (type(s) != new_sym("socket"))
    RAISE << "not a socket: " << s << '\n' << die();
  return (socket_type*)to_int(car(cdr(cdr(s))));
}

COMPILE_FN(socket_fd, compiledfn_socket_fd, "($sock)",
  return mkref(new_num(to_socket(lookup("$sock"))->fd));
)

COMPILE_FN(socket, compiledfn_socket, "($host $port)",
  socket_type* sock = new socket_type();
  sock->fd = socket(AF_INET, SOCK_STREAM, 0);
  if (sock->fd < 0) perror("socket() failed");
  bzero(&sock->addr, sizeof(sock->addr));
  sock->addr.sin_family = AF_INET;
  hostent* host = gethostbyname(to_string(lookup("$host")).c_str());
  bcopy((char*)host->h_addr, (char*)&sock->addr.sin_addr.s_addr, host->h_length);
  sock->addr.sin_port = htons(to_int(lookup("$port")));
  PERR(connect(sock->fd, (sockaddr*)&sock->addr, sizeof(sock->addr)));
  return mkref(new_socket(sock));
)

COMPILE_FN(server_socket, compiledfn_serversocket, "($port)",
  socket_type* sock = new socket_type();
  sock->fd = socket(AF_INET, SOCK_STREAM, 0);
  if (sock->fd < 0) perror("sock() failed");
  int dummy;
  PERR(setsockopt(sock->fd, SOL_SOCKET, SO_REUSEADDR, &dummy, sizeof(dummy)));
  bzero(&sock->addr, sizeof(sock->addr));
  sock->addr.sin_family = AF_INET;   sock->addr.sin_addr.s_addr = INADDR_ANY;
  sock->addr.sin_port = htons(to_int(lookup("$port")));
  PERR(bind(sock->fd, (sockaddr*)&sock->addr, sizeof(sock->addr)));
  PERR(listen(sock->fd, 5));
  return mkref(new_socket(sock));
)

COMPILE_FN(accept, compiledfn_accept, "($sock)",
  socket_type* sock = to_socket(lookup("$sock"));
  socket_type* clientsock = new socket_type();  socklen_t n = sizeof(clientsock->addr);
  bzero(&clientsock->addr, n);
  clientsock->fd = accept(sock->fd, (sockaddr*)&clientsock->addr, &n);
  if (clientsock->fd < 0) perror("bind");
  return mkref(new_socket(clientsock));
)

COMPILE_FN(close_socket, compiledfn_close_socket, "($sock)",
  socket_type* sock = to_socket(lookup("$sock"));
  close(sock->fd);
  delete sock;
  set_car(cdr(cdr(lookup("$sock"))), nil);
  return nil;
)



#include<signal.h>

struct sigaction Original_signal_handler;
void interrupt(int s) {
  cell* f = lookup_dynamic_binding(new_sym("on_interrupt"));
  if (f)
    eval(new_cons(f));  // leak
  if (Original_signal_handler.sa_handler)
    Original_signal_handler.sa_handler(s);
}

void catch_ctrl_c() {
  struct sigaction curr;
  curr.sa_handler = interrupt;
  sigemptyset(&curr.sa_mask);
  curr.sa_flags = 0;
  sigaction(SIGINT, NULL, &Original_signal_handler);
  sigaction(SIGINT, &curr, NULL);
}
