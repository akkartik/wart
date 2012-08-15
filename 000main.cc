//// interpreter skeleton: either run tests or read-eval-print loop

// Whitespace-sensitivity requires remembering indent state across reads.
struct CodeStream {
  istream& fd;
  long currIndent;

  CodeStream(istream& in) :fd(in), currIndent(-1) {
    fd >> std::noskipws;
  }
};

Cell* read(CodeStream&);
Cell* eval(Cell*);
ostream& operator<<(ostream&, Cell*);

bool interactive = false;   // trigger eval on empty lines

extern unsigned long numAllocs;

int main(int argc, unused char* argv[]) {
  if (argc > 1) {
    runTests();
    return 0;
  }

  setup();
  loadFiles(".wart");

  interactive = true;
  catchCtrlC();

  CodeStream cs(cin);
  while (true) {
    cout << numUnfreed() << " " << numAllocs << " " << "wart> ";
    Cell* form = read(cs);
    if (cin.eof()) break;
    Cell* result = eval(form);
    cout << result << endl;
    rmref(result);
    rmref(form);
    reset(cin);
  }
  return 0;
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

Cell* read(CodeStream& c) {
  return mkref(transform(nextRawCell(c)));
}

void reset(istream& in) {
  in.get();
  if (interactive) in.get();
}

// helper to read from string
stringstream& stream(string s) {
  stringstream& result = *new stringstream(s);
  result << std::noskipws;
  return result;
}
