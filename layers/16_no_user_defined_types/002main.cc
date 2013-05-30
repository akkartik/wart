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
  interactive_setup();
  load_files(".wart");
  cout << "ready! type in an expression, then hit enter twice. ctrl-d exits.\n";
  while (!cin.eof()) {
    list<cell*> forms = read_all(cin);
    for (list<cell*>::iterator p = forms.begin(); p != forms.end(); ++p) {
      cell* result = eval(*p);
      cout << "=> " << result << '\n';
      rmref(result);
      rmref(*p);
    }
  }
}

//// read: tokenize, segment, parse, build cells
cell* read(istream& in) {
  return mkref(next_cell(in));
}

// parse a paragraph of expressions until empty line
list<cell*> read_all(istream& in) {
  list<cell*> results;
  do {
    results.push_back(read(in));
  } while (!in.eof() && in.peek() != '\n');
  return results;
}



//// test harness

bool Running_tests = false;

typedef void (*test_fn)(void);

const test_fn Tests[] = {
  #include "test_list"
};

long Num_failures = 0;
bool Passed = true;

#define CHECK(X) if (!(X)) { \
    ++Num_failures; \
    cerr << "\nF " << __FUNCTION__ << ": " << #X << '\n'; \
    Passed = false; \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

#define CHECK_EQ(X, Y) if ((X) != (Y)) { \
    ++Num_failures; \
    cerr << "\nF " << __FUNCTION__ << ": " << #X << " == " << #Y << '\n'; \
    cerr << "  got " << (X) << '\n';  /* BEWARE: multiple eval */ \
    Passed = false; \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

void run_tests() {
  Running_tests = true;
  Pretend_raise = true;  // for death tests
  time_t t; time(&t);
  cerr << "C tests: " << ctime(&t);
  for (unsigned long i=0; i < sizeof(Tests)/sizeof(Tests[0]); ++i) {
    setup();
    (*Tests[i])();
    verify();
  }

  Pretend_raise = false;
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
  teardown_cells();
  if (!Passed) return;
  if (Raise_count != 0) cerr << Raise_count << " errors encountered\n";
  if (num_unfreed() > 0) dump_unfreed();
}

// helper to read from string
// leaks memory; just for convenient tests
cell* read(string s) {
  return read(*new stringstream(s));
}



void setup() {
  setup_cells();
  setup_common_syms();
  Raise_count = 0;
  Passed = true;
}

bool Interactive = false;
void interactive_setup() {
  setup();
  Interactive = true;
}
