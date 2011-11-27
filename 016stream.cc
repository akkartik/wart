Cell* newIstream(istream* x) {
  return newCons(newSym("type"), newCons(newSym("stream"),
            newCons(newNum((long)x), nil)));
}

Cell* newOstream(ostream* x) {
  return newCons(newSym("type"), newCons(newSym("stream"),
            newCons(newNum((long)x), nil)));
}

istream& toIstream(Cell* x) {
  if (!isCons(x) || car(x) != newSym("type") || car(cdr(x)) != newSym("stream"))
    ERR << "not a stream: " << x << endl << DIE;
  return *(istream*)toNum(car(cdr(cdr(x))));
}

ostream& toOstream(Cell* x) {
  if (!isCons(x) || car(x) != newSym("type") || car(cdr(x)) != newSym("stream"))
    ERR << "not a stream: " << x << endl << DIE;
  return *(ostream*)toNum(car(cdr(cdr(x))));
}



#define STDIN dynamics[newSym("stdin")].top()
#define STDOUT dynamics[newSym("stdout")].top()
#define STDERR dynamics[newSym("stderr")].top()

void setupStreams() {
  newDynamicScope(newSym("stdin"), newIstream(&cin));
  newDynamicScope(newSym("stdout"), newOstream(&cout));
  newDynamicScope(newSym("stderr"), newOstream(&cerr));
}

void teardownStreams() {
  endDynamicScope(newSym("stdin"));
  endDynamicScope(newSym("stdout"));
  endDynamicScope(newSym("stderr"));
}



COMPILE_PRIM_FUNC(infile, primFunc_infile, "($name)",
  return mkref(newIstream(new ifstream(toString(lookup("$name")).c_str(), std::ios::binary)));
)

COMPILE_PRIM_FUNC(close_infile, primFunc_close_infile, "($stream)",
  ifstream* f = (ifstream*)toNum(car(cdr(cdr(lookup("$stream")))));
  f->close();
  delete f;
  return nil;
)

COMPILE_PRIM_FUNC(outfile, primFunc_outfile, "($name)",
  return mkref(newOstream(new ofstream(toString(lookup("$name")).c_str(), std::ios::binary)));
)

COMPILE_PRIM_FUNC(close_outfile, primFunc_close_outfile, "($stream)",
  ofstream* f = (ofstream*)toNum(car(cdr(cdr(lookup("$stream")))));
  f->close();
  delete f;
  return nil;
)

COMPILE_PRIM_FUNC(instring, primFunc_instring, "($s)",
  return mkref(newIstream(new stringstream(toString(lookup("$s")))));
)

COMPILE_PRIM_FUNC(outstring, primFunc_outstring, "()",
  return mkref(newOstream(new ostringstream()));
)

COMPILE_PRIM_FUNC(outstring_buffer, primFunc_outstring_buffer, "($stream)",
  ostringstream* s = (ostringstream*)toNum(car(cdr(cdr(lookup("$stream")))));
  return mkref(newString(s->str()));
)



#include<fcntl.h>

COMPILE_PRIM_FUNC(input_fd, primFunc_input_fd, "($name)",
  return mkref(newNum(open(toString(lookup("$name")).c_str(), O_RDONLY)));
)

COMPILE_PRIM_FUNC(output_fd, primFunc_output_fd, "($name)",
  return mkref(newNum(open(toString(lookup("$name")).c_str(), O_WRONLY)));
)

// buffer a file descriptor
struct FdStreamBuf :public std::streambuf {
public:
   int fd;
   char inBuffer[BUFSIZ];
   char outBuffer[BUFSIZ];

   FdStreamBuf(int f) {
      fd = f;
      setg(inBuffer, inBuffer, inBuffer);
      setp(outBuffer, outBuffer+BUFSIZ);
   }
   ~FdStreamBuf() {
     close(fd);
   }

   int underflow() {
      if (gptr() < egptr())
        return *(unsigned char*)gptr();

      int n = read(fd, inBuffer, BUFSIZ);
      if (n <= 0) return EOF;

      setg(inBuffer, inBuffer, inBuffer+n);
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

      setp(outBuffer, outBuffer+BUFSIZ);
      return 0;
   }
};

COMPILE_PRIM_FUNC(infd, primFunc_infd, "($fd)",
  return mkref(newIstream(new iostream(new FdStreamBuf(toNum(lookup("$fd")))))); // leak
)

COMPILE_PRIM_FUNC(outfd, primFunc_outfd, "($fd)",
  return mkref(newOstream(new iostream(new FdStreamBuf(toNum(lookup("$fd")))))); // leak
)

COMPILE_PRIM_FUNC(close, primFunc_close, "($fd)",
  close(toNum(lookup("$fd")));
  return nil;
)
