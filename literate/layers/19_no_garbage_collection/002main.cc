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

int main(int argc, unused char* argv[]) {
  if (argc > 1) {
    run_tests();
    return 0;
  }

  //// Interactive loop: parse commands from user, evaluate them, print the results
  setup();
  load_files(".wart");
  cout << "ready! type in an expression, then hit enter twice. ctrl-d exits.\n";
  while (!cin.eof()) {
    cout << "=> " << eval(read(cin)) << '\n';
  }
}

//// read: tokenize, segment, parse, build cells
cell* read(istream& in) {
  return next_cell(in);
}

extern cell* nil;

cell* run(istream& in) {
  cell* result = nil;
  do {
      result = eval(read(in));
  } while (!eof(in));
  return result;
}

// parse a paragraph of expressions until empty line
list<cell*> read_all(istream& in) {
  list<cell*> results;
  do {
      results.push_back(read(in));
  } while (!eof(in));
  return results;
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
  teardown_bindings();
  if (!Passed)
    ;
  else
    cerr << ".";
}

void setup() {
  setup_cells();
  setup_common_syms();
  Hide_warnings = false;
  Passed = true;
}



//// helpers for tests

list<cell*> read_all(string s) {
  stringstream in(s);
  return read_all(in);
}

cell* run(string s) {
  stringstream in(s);
  return run(in);
}
