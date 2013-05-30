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

bool Warn_on_unknown_var = false;

int main(int argc, unused char* argv[]) {
  if (argc > 1) {
    run_tests();
    return 0;
  }

  //// Interactive loop: parse commands from user, evaluate them, print the results
  interactive_setup();
  load_files(".wart");
  Warn_on_unknown_var = true;
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

//// read: tokenize, parenthesize, parse, transform infix, build cells, transform $vars
cell* read(indent_sensitive_stream& in) {
  return mkref(transform_dollar_vars(next_cell(in)));
}

// wart does paren-insertion, which requires some extra state.
struct indent_sensitive_stream {
  istream& fd;
  bool at_start_of_line;
  explicit indent_sensitive_stream(istream& in) :fd(in), at_start_of_line(true) { fd >> std::noskipws; }
  // leaky version just for convenient tests
  explicit indent_sensitive_stream(string s) :fd(*new stringstream(s)), at_start_of_line(true) { fd >> std::noskipws; }
  bool eof() { return fd.eof(); }
};

// parse a paragraph of expressions until empty line
list<cell*> read_all(istream& fd) {
  indent_sensitive_stream in(fd);
  list<cell*> results;
  do {
    results.push_back(read(in));
  } while (!in.at_start_of_line && fd.peek() != '\n' && !fd.eof());
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
  teardown_streams();
  teardown_compiledfns();
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
  setup_scopes();
  setup_compiledfns();
  setup_streams();
  Raise_count = 0;
  Passed = true;
}

bool Interactive = false;
void interactive_setup() {
  setup();
  Interactive = true;
  catch_ctrl_c();
}
