//// run unit tests or interactive interpreter

// The design of lisp seems mindful of the following:
//  keep programs as short as possible
//  as programs grow, allow them to be decomposed into functions
//  functions are defined with a recipe for work, and invoked to perform that work
//  functions are parameterized with vars (parameters) and invoked with data (arguments)
//  new functions can be defined at any time
//  functions can invoke other functions before they've been defined
//  functions are data; they can be passed to other functions
//  when an invocation contains multiple functions, make the one being invoked easy to identify
//    almost always the first symbol in the invocation
//  there have to be *some* bedrock primitives, but any primitive can be user-defined in principle
//
// These weren't the reasons lisp was created; they're the reasons I attribute
// to its power.

bool Interactive = false;

int main(int argc, unused char* argv[]) {
  if (argc > 1) {
    run_tests();
    return 0;
  }

  //// Interactive loop: parse commands from user, evaluate them, print the results
  Interactive = true;
  setup();
  load_files(".wart");
  cout << "ready! type in an expression, then hit enter twice. ctrl-d exits.\n";
  while (!cin.eof()) {
    cout << "=> " << run(cin) << '\n';
  }
}

//// read: tokenize, parenthesize, parse, transform infix, build cells, transform $vars
cell* read(indent_sensitive_stream& in) {
  return mkref(transform_dollar_vars(next_cell(in)));
}

// parenthesize requires input stream to know when it's at start of line
struct indent_sensitive_stream {
  istream& fd;
  bool at_start_of_line;
  explicit indent_sensitive_stream(istream& in) :fd(in), at_start_of_line(true) { fd >> std::noskipws; }
  // leaky version just for convenient tests
  explicit indent_sensitive_stream(string s) :fd(*new stringstream(s)), at_start_of_line(true) { fd >> std::noskipws; }
  bool eof() { return fd.eof(); }
};

extern cell* nil;

// In batch mode, evaluate all exprs in input.
// In interactive mode, evaluate all exprs until empty line.
// Return value of last expr.
cell* run(istream& i) {
  indent_sensitive_stream in(i);
  cell* result = nil;
  do {
      cell* form = read(in);
      update(result, eval(form));
      rmref(form);
  } while (!eof(in.fd) && (!Interactive || in.fd.peek() != '\n'));
  return result;
}

bool eof(istream& in) {
  in.peek();
  return in.eof();
}



//// test harness

void run_tests() {
  time_t t; time(&t);
  cerr << "C tests: " << ctime(&t);
  for (unsigned long i=0; i < sizeof(Tests)/sizeof(Tests[0]); ++i) {
    START_TRACING_UNTIL_END_OF_SCOPE;
    setup();
    (*Tests[i])();
    verify();
  }

  setup();
  load_files(".wart");   // after GC tests
  load_files(".test");

  cerr << '\n';
  if (Num_failures > 0)
    cerr << Num_failures << " failure"
         << (Num_failures > 1 ? "s" : "")
         << '\n';
}

void verify() {
  Hide_warnings = false;
  teardown_streams();
  teardown_compiledfns();
  teardown_cells();
  if (!Passed)
    ;
  else if (num_unfreed() > 0)
    dump_unfreed();
  else
    cerr << ".";
}

void setup() {
  setup_cells();
  setup_common_syms();
  setup_scopes();
  setup_compiledfns();
  setup_streams();
  Hide_warnings = false;
  Passed = true;
}



//// helpers for tests

void read_all(string s) {
  stringstream ss(s);
  indent_sensitive_stream in(ss);
  do {
      rmref(read(in));
  } while (!eof(in.fd));
  // return nothing; we'll just verify the trace
}

void run(string s) {
  stringstream in(s);
  rmref(run(in));
  // return nothing; we'll just verify the trace
}

cell* read(string s) {
  return read(*new stringstream(s));
}
