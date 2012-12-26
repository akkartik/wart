//// compiled primitives for files and streams

Cell* newIstream(istream* x) {
  return newObject("stream", newNum((long)x));
}

Cell* newOstream(ostream* x) {
  return newObject("stream", newNum((long)x));
}

istream& toIstream(Cell* x) {
  if (type(x) != newSym("stream"))
    RAISE << "not a stream: " << x << endl << DIE;
  return *(istream*)toInt(car(cdr(cdr(x))));
}

ostream& toOstream(Cell* x) {
  if (type(x) != newSym("stream"))
    RAISE << "not a stream: " << x << endl << DIE;
  return *(ostream*)toInt(car(cdr(cdr(x))));
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



COMPILE_FN(infile, compiledFn_infile, "($name)",
  return mkref(newIstream(new ifstream(toString(lookup("$name")).c_str(), std::ios::binary)));
)

COMPILE_FN(close_infile, compiledFn_close_infile, "($stream)",
  ifstream* f = (ifstream*)toInt(rep(lookup("$stream")));
  f->close();
  delete f;
  return nil;
)

COMPILE_FN(outfile, compiledFn_outfile, "($name)",
  return mkref(newOstream(new ofstream(toString(lookup("$name")).c_str(), std::ios::binary)));
)

COMPILE_FN(close_outfile, compiledFn_close_outfile, "($stream)",
  ofstream* f = (ofstream*)toInt(rep(lookup("$stream")));
  f->close();
  delete f;
  return nil;
)

COMPILE_FN(instring, compiledFn_instring, "($s)",
  return mkref(newIstream(new stringstream(toString(lookup("$s")))));
)

COMPILE_FN(outstring, compiledFn_outstring, "()",
  return mkref(newOstream(new ostringstream()));
)

COMPILE_FN(outstring_buffer, compiledFn_outstring_buffer, "($stream)",
  ostringstream* s = (ostringstream*)toInt(car(cdr(cdr(lookup("$stream")))));
  return mkref(newString(s->str()));
)



#include<fcntl.h>
#include<unistd.h>

COMPILE_FN(input_fd, compiledFn_input_fd, "($name)",
  return mkref(newNum(open(toString(lookup("$name")).c_str(), O_RDONLY)));
)

COMPILE_FN(output_fd, compiledFn_output_fd, "($name)",
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

COMPILE_FN(infd, compiledFn_infd, "($fd)",
  return mkref(newIstream(new iostream(new FdStreamBuf(toInt(lookup("$fd"))))));  // leak
)

COMPILE_FN(outfd, compiledFn_outfd, "($fd)",
  return mkref(newOstream(new iostream(new FdStreamBuf(toInt(lookup("$fd"))))));  // leak
)

COMPILE_FN(close, compiledFn_close, "($stream)",
  close(toInt(rep(lookup("$stream"))));
  return nil;
)
