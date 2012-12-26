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

// To satisfy these properties, lisp is designed around function invocation as
// the core activity of the language; features in other languages are
// definable as functions. Even large programs are structured as a single
// function that performs all the work when invoked.
//
// Setting up such a function involves two stages:
//  - setting up the data for it from disk to memory, including subsidiary functions (read)
//  - invoking this function by repeatedly invoking its subsidiary functions (eval)
// A function can be read once into memory, and eval'd any number of times
// once read.
//
// Read is the time for optimizations, when subsidiary functions can be
// specialized to a specific call-site.

int main(int argc, unused char* argv[]) {
  if (argc > 1) {
    runTests();
    return 0;
  }

  setup();
  loadFiles(".wart");
}

void loadFile(const char* filename) {
  ifstream f(filename);
  if (f.fail()) return;
  //// read: tokenize, parenthesize, parse, transform infix, build cells, transform $vars
  list<Cell*> exprs = transformDollarVars(buildCells(transformInfix(parse(insertImplicitParens(tokenize(f))))));
  //// eval
  for (list<Cell*>::iterator p = exprs.begin(); p != exprs.end(); ++p) {
//?     cerr << *p << endl;   // uncomment this to track down errors in wart files
    rmref(eval(*p));
    rmref(*p);
  }
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
  if (numFailures == 0) return;
  cerr << numFailures << " failure";
      if (numFailures > 1) cerr << "s";
      cerr << endl;
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
  return mkref(transformDollarVars(buildCells(transformInfix(parse(insertImplicitParens(tokenize(*new stringstream(s))))))).front());
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