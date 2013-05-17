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

  readAll(cin);
}

// TODO: logically belongs in parse.cc
struct AstNode {
  Token atom;
  list<AstNode> elems;

  explicit AstNode(Token t) :atom(t) {}
  explicit AstNode(list<AstNode> l) :elems(l) {}

  bool operator==(const string& x) const {
    return elems.empty() && atom == x;
  }
  bool operator!=(const string& x) const {
    return !(*this == x);
  }
};

void readAll(istream& in) {
  do {
    nextAstNode(in);
  } while (!eof(in));
}

bool eof(istream& in) {
  in.peek();
  return in.eof();
}



//// test harness

typedef void (*TestFn)(void);

const TestFn tests[] = {
  #include "test_list"
};

#define CHECK(X) if (!(X)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << endl; \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

void runTests() {
  pretendRaise = true;  // for death tests
  time_t t; time(&t);
  cerr << "C tests: " << ctime(&t);
  for (unsigned long i=0; i < sizeof(tests)/sizeof(tests[0]); ++i) {
    START_TRACING_UNTIL_END_OF_SCOPE;
    (*tests[i])();
  }

  cerr << endl;
  if (numFailures > 0)
    cerr << numFailures << " failure"
         << (numFailures > 1 ? "s" : "")
         << endl;
}
