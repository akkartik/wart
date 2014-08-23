string Last_file = "";
int main(int argc, const char* argv[]) {
  Last_file = flag_value("--until", argc, argv);
  if (flag("test", argc, argv))
    return run_tests();
  return tangle_files_in_cwd();
}

cell* read(istream& in) {
  return mkref(next_cell(in));
}

bool eof(istream& in) {
  in.peek();
  return in.eof();
}

bool flag(const string& flag, int argc, const char* argv[]) {
  for (int i = 1; i < argc; ++i)
    if (string(argv[i]) == flag)
      return true;
  return false;
}

string flag_value(const string& flag, int argc, const char* argv[]) {
  for (int i = 1; i < argc-1; ++i)
    if (string(argv[i]) == flag)
      return argv[i+1];
  return "";
}



//// test harness

int run_tests() {
  time_t t; time(&t);
  cerr << "C tests: " << ctime(&t);
  for (unsigned long i=0; i < sizeof(Tests)/sizeof(Tests[0]); ++i) {
    START_TRACING_UNTIL_END_OF_SCOPE;
    setup();
    (*Tests[i])();
    verify();
  }

  cerr << '\n';
  if (Num_failures > 0)
    cerr << Num_failures << " failure"
         << (Num_failures > 1 ? "s" : "")
         << '\n';
  return Num_failures;
}

void verify() {
  Hide_warnings = false;
  teardown_cells();
  if (!Passed)
    ;
  else if (num_unfreed() > 0)
    dump_unfreed();
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

void read_all(string s) {
  stringstream in(s);
  do {
      rmref(read(in));
  } while (!eof(in));
  // return nothing; we'll just verify the trace
}

cell* read(string s) {
  return read(*new stringstream(s));
}
