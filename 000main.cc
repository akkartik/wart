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

bool interactive = false; // trigger eval on empty lines

extern unsigned long numAllocs;

int main(int argc, unused char* argv[]) {
  if (argc > 1) {
    runTests();
    return 0;
  }

  init();
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
