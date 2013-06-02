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

  read_all(cin);
}

// TODO: logically belongs in parse.cc
struct ast_node {
  token atom;
  list<ast_node> elems;

  explicit ast_node(token t) :atom(t) {}
  explicit ast_node(list<ast_node> l) :elems(l) {}

  bool operator==(const string& x) const {
    return elems.empty() && atom == x;
  }
  bool operator!=(const string& x) const {
    return !(*this == x);
  }
};

void read_all(istream& in) {
  do {
      next_ast_node(in);
  } while (!eof(in));
}

bool eof(istream& in) {
  in.peek();
  return in.eof();
}



//// test harness

typedef void (*test_fn)(void);

const test_fn Tests[] = {
  #include "test_list"
};

#define CHECK(X) if (!(X)) { \
    ++Num_failures; \
    cerr << "\nF " << __FUNCTION__ << ": " << #X << '\n'; \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

void run_tests() {
  Do_raise = false;  // for death tests
  time_t t; time(&t);
  cerr << "C tests: " << ctime(&t);
  for (unsigned long i=0; i < sizeof(Tests)/sizeof(Tests[0]); ++i) {
    START_TRACING_UNTIL_END_OF_SCOPE;
    (*Tests[i])();
  }

  cerr << '\n';
  if (Num_failures > 0)
    cerr << Num_failures << " failure"
         << (Num_failures > 1 ? "s" : "")
         << '\n';
}
