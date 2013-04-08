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
    runTests();
    return 0;
  }

  //// Interactive loop: parse commands from user, evaluate them, print the results
  interactive_setup();
  loadFiles(".wart");
  cout << "ready! type in an expression, then hit enter twice. ctrl-d exits.\n";
  while (true) {
    list<Cell*> forms = readAll(cin);
    if (cin.eof()) return 0;
    for (list<Cell*>::iterator p = forms.begin(); p != forms.end(); ++p) {
      Cell* result = eval(*p);
      cout << "=> " << result << endl;
      rmref(result);
      rmref(*p);
    }
  }
}

//// read: tokenize, parenthesize, parse, transform infix, build cells, transform $vars
Cell* read(IndentSensitiveStream& in) {
  return mkref(transformDollarVars(nextCell(in)));
}

// wart does paren-insertion, which requires some extra state.
struct IndentSensitiveStream {
  istream& fd;
  bool atStartOfLine;
  explicit IndentSensitiveStream(istream& in) :fd(in), atStartOfLine(true) { fd >> std::noskipws; }
  // leaky version just for convenient tests
  explicit IndentSensitiveStream(string s) :fd(*new stringstream(s)), atStartOfLine(true) { fd >> std::noskipws; }
  bool eof() { return fd.eof(); }
} STDIN(cin);

list<Cell*> readAll(istream& fd) {
  IndentSensitiveStream in(fd);
  list<Cell*> results;
  do {
    results.push_back(read(in));
  } while (!in.atStartOfLine && fd.peek() != '\n' && !fd.eof());
  return results;
}



//// test harness

bool runningTests = false;

typedef void (*TestFn)(void);

const TestFn tests[] = {
  #include "test_list"
};

long numFailures = 0;
bool passed = true;

#define check(X) if (!(X)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << endl; \
    passed = false; \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

#define checkEq(X, Y) if ((X) != (Y)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << " == " << #Y << endl; \
    cerr << "  got " << (X) << endl;  /* BEWARE: multiple eval */ \
    passed = false; \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

void runTests() {
  runningTests = true;
  pretendRaise = true;  // for death tests
  time_t t; time(&t);
  cerr << "C tests: " << ctime(&t);
  for (unsigned long i=0; i < sizeof(tests)/sizeof(tests[0]); ++i) {
    setup();
    (*tests[i])();
    verify();
  }

  pretendRaise = false;
  setup();
  loadFiles(".wart");   // after GC tests
  loadFiles(".test");

  cerr << endl;
  if (numFailures > 0)
    cerr << numFailures << " failure"
         << (numFailures > 1 ? "s" : "")
         << endl;
}

void verify() {
  teardownStreams();
  teardownCompiledFns();
  teardownCells();
  if (!passed) return;
  if (raiseCount != 0) cerr << raiseCount << " errors encountered" << endl;
  if (numUnfreed() > 0) dumpUnfreed();
}

// helper to read from string
// leaks memory; just for convenient tests
Cell* read(string s) {
  return read(*new stringstream(s));
}



void setup() {
  setupCells();
  setupCommonSyms();
  setupScopes();
  setupCompiledFns();
  setupStreams();
  raiseCount = 0;
  passed = true;
}

bool interactive = false;
void interactive_setup() {
  setup();
  interactive = true;
  catchCtrlC();
}
