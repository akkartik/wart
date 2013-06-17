//// compiled primitives for files and streams

// Design considered the following:
//   permit cascading of close operations
//     so return non-nil on success

cell* new_istream(istream* x) {
  return new_object("istream", new_num((long)x));
}

cell* new_ostream(ostream* x) {
  return new_object("ostream", new_num((long)x));
}

istream& to_istream(cell* x) {
  if (type(x) != new_sym("istream"))
    RAISE << "not an istream: " << x << '\n' << die();
  return *(istream*)to_int(car(cdr(cdr(x))));
}

ostream& to_ostream(cell* x) {
  if (type(x) != new_sym("ostream"))
    RAISE << "not an ostream: " << x << '\n' << die();
  return *(ostream*)to_int(car(cdr(cdr(x))));
}



#define STDIN Dynamics[new_sym("stdin")].top()
#define STDOUT Dynamics[new_sym("stdout")].top()
#define STDERR Dynamics[new_sym("stderr")].top()

void setup_streams() {
  new_dynamic_scope(new_sym("stdin"), new_istream(&cin));
  new_dynamic_scope(new_sym("stdout"), new_ostream(&cout));
  new_dynamic_scope(new_sym("stderr"), new_ostream(&cerr));
}

void teardown_streams() {
  end_dynamic_scope(new_sym("stdin"));
  end_dynamic_scope(new_sym("stdout"));
  end_dynamic_scope(new_sym("stderr"));
}



COMPILE_FN(infile, compiledfn_infile, "($name)",
  return mkref(new_istream(new ifstream(to_string(lookup("$name")).c_str(), std::ios::binary)));
)

COMPILE_FN(close_infile, compiledfn_close_infile, "($stream)",
  ifstream* f = (ifstream*)to_int(rep(lookup("$stream")));
  f->close();
  delete f;
  set_car(cdr(cdr(lookup("$stream"))), nil);
  return mkref(lookup("$stream"));
)

COMPILE_FN(outfile, compiledfn_outfile, "($name)",
  return mkref(new_ostream(new ofstream(to_string(lookup("$name")).c_str(), std::ios::binary)));
)

COMPILE_FN(close_outfile, compiledfn_close_outfile, "($stream)",
  ofstream* f = (ofstream*)to_int(rep(lookup("$stream")));
  f->close();
  delete f;
  set_car(cdr(cdr(lookup("$stream"))), nil);
  return mkref(lookup("$stream"));
)

COMPILE_FN(instring, compiledfn_instring, "($s)",
  return mkref(new_istream(new stringstream(to_string(lookup("$s")))));
)

COMPILE_FN(outstring, compiledfn_outstring, "()",
  return mkref(new_ostream(new ostringstream()));
)

COMPILE_FN(outstring_buffer, compiledfn_outstring_buffer, "($stream)",
  ostringstream* s = (ostringstream*)to_int(car(cdr(cdr(lookup("$stream")))));
  return mkref(new_string(s->str()));
)



#include<fcntl.h>
#include<unistd.h>

COMPILE_FN(input_fd, compiledfn_input_fd, "($name)",
  return mkref(new_num(open(to_string(lookup("$name")).c_str(), O_RDONLY)));
)

COMPILE_FN(output_fd, compiledfn_output_fd, "($name)",
  return mkref(new_num(open(to_string(lookup("$name")).c_str(), O_WRONLY)));
)

// buffer a file descriptor
struct fd_streambuf :public std::streambuf {
public:
   int fd;
   char in_buffer[BUFSIZ];
   char out_buffer[BUFSIZ];

   fd_streambuf(int f) {
      fd = f;
      setg(in_buffer, in_buffer, in_buffer);
      setp(out_buffer, out_buffer+BUFSIZ);
   }
   ~fd_streambuf() {
     close(fd);
   }

   int underflow() {
      if (gptr() < egptr())
        return *(unsigned char*)gptr();

      int n = read(fd, in_buffer, BUFSIZ);
      if (n <= 0) return EOF;

      setg(in_buffer, in_buffer, in_buffer+n);
      return *(unsigned char*)gptr();
   }

   int overflow(unused int c) {
      return EOF;
   }

   int sync() {
      if (pbase() == pptr())
         return 0;

      int n = write(fd, pbase(), pptr()-pbase());
      if (n < 0) return n;

      setp(out_buffer, out_buffer+BUFSIZ);
      return 0;
   }
};

COMPILE_FN(infd, compiledfn_infd, "($fd)",
  return mkref(new_istream(new iostream(new fd_streambuf(to_int(lookup("$fd"))))));  // leak
)

COMPILE_FN(outfd, compiledfn_outfd, "($fd)",
  return mkref(new_ostream(new iostream(new fd_streambuf(to_int(lookup("$fd"))))));  // leak
)

COMPILE_FN(close, compiledfn_close, "($stream)",
  close(to_int(rep(lookup("$stream"))));
  return mkref(lookup("$stream"));
)
