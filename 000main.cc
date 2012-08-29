//// run unit tests or interactive interpreter

// Whitespace-sensitivity requires remembering indent state across reads.
struct CodeStream {
  istream& fd;
  long currIndent;

  CodeStream(istream& in) :fd(in), currIndent(-1) {
    fd >> std::noskipws;
  }

  bool eof() { return fd.eof(); }
};
CodeStream STDIN(cin);

int main(int argc, unused char* argv[]) {
  if (argc > 1) {
    runTests();
    return 0;
  }

  // Interpreter loop: prompt, read, eval, print
  interactive_setup();
  while (true) {
    prompt("wart> ");
    Cell* form = read(STDIN);
    if (STDIN.eof()) break;
    Cell* result = eval(form);
    cout << result << endl;

    rmref(result);
    rmref(form);
    reset(STDIN);
  }
  return 0;
}

// read: tokenize, parenthesize, parse, build cells, transform
Cell* read(CodeStream& c) {
  return mkref(transform(nextRawCell(c)));
}



// test harness

bool runningTests = false;
long numFailures = 0;

typedef void (*TestFn)(void);

const TestFn tests[] = {
  #include "test_list"
};

void runTests() {
  runningTests = true;
  pretendRaise = true;  // for death tests
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
  if (raiseCount != 0) cerr << raiseCount << " errors encountered" << endl;
  teardownStreams();
  teardownCompiledFns();
  teardownCells();

  if (numUnfreed() > 0) {
    RAISE << "Memory leak!\n";
    dumpUnfreed();
  }
}

void setup() {
  setupCells();
  setupScopes();
  setupCompiledFns();
  setupStreams();
  raiseCount = 0;
}



// misc

bool interactive = false;   // trigger eval on empty lines
void interactive_setup() {
  setup();
  loadFiles(".wart");
  interactive = true;
  catchCtrlC();
}

void prompt(string msg) {
  extern unsigned long numAllocs;
  extern long numUnfreed();
  cout << numUnfreed() << " " << numAllocs << " " << msg;
}

void reset(CodeStream& in) {
  in.fd.get();
  if (interactive) in.fd.get();
}

// helper to read from string
stringstream& stream(string s) {
  stringstream& result = *new stringstream(s);
  result << std::noskipws;
  return result;
}
