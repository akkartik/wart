//// run unit tests or interactive interpreter

bool Interactive = false;
bool Warn_on_unknown_var = true;

// commandline args
string Last_file = "";

int main(int argc, const char* argv[]) {
  Last_file = flag_value("--until", argc, argv);
  if (flag("test", argc, argv))
    return run_tests();
  if (flag("tangle", argc, argv))
    return tangle_files_in_cwd();

  //// Interactive loop: parse commands from user, evaluate them, print the results
  setup();
  cerr << time_string() << " loading wart files       (takes ~15 seconds)\n";
  load_files(".wart");
  cerr << time_string() << " ready! type in an expression, then hit enter twice. ctrl-d exits.\n";
  Interactive = true;  // stop run on two enters
  while (!cin.eof()) {
    cell* curr = run(cin);
    cout << "=> " << curr << '\n';
    rmref(curr);
  }
}

//// read: tokenize, parenthesize, parse, transform infix, build cells, transform $vars
// simply returns nil on eof
cell* read(indent_sensitive_stream& in) {
  return mkref(transform_dollar_vars(next_cell(in)));
}

// parenthesize requires input stream to know when it's at start of line
struct indent_sensitive_stream {
  istream& fd;
  bool at_start_of_line;
  explicit indent_sensitive_stream(istream& in) :fd(in), at_start_of_line(true) { fd >> std::noskipws; }
  // leaky version just for convenient tests
  explicit indent_sensitive_stream(string s) :fd(*new stringstream(s)), at_start_of_line(true) { fd >> std::noskipws; }
  bool eof() { return fd.eof(); }
};

extern cell* nil;

cell* run(istream& i) {
  indent_sensitive_stream in(i);
  cell* result = nil;
  while (true) {
    cell* form = read(in);
    update(result, eval(form));
    rmref(form);
    if (in.eof()) break;
    if (Interactive && at_end_of_line(in))
      // 'in' has no state left; destroy and print
      break;
  }
  return result;
}

bool at_end_of_line(indent_sensitive_stream& in) {
  if (in.at_start_of_line) return true;
  skip_whitespace(in.fd);
  if (in.fd.peek() == '#')
    skip_comment(in.fd);
  return in.fd.peek() == '\n';
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
  cerr << time_string() << " C tests\n";
  for (unsigned long i=0; i < sizeof(Tests)/sizeof(Tests[0]); ++i) {
    START_TRACING_UNTIL_END_OF_SCOPE;
    setup();
    CLEAR_TRACE;
//?     Trace_stream->dump_layer = "test";  // uncomment to segment test output
    (*Tests[i])();
    verify();
  }

  setup();
  cerr << "\n" << time_string() << " loading wart files       (takes ~15 seconds)\n";
  load_files(".wart");  // after GC tests
  cerr << time_string() << " wart tests\n";
  load_files(".test");

  cerr << '\n';
  if (Num_failures > 0)
    cerr << Num_failures << " failure"
         << (Num_failures > 1 ? "s" : "")
         << '\n';
  return Num_failures;
}

void verify() {
  Hide_warnings = false;
  teardown_streams();
  teardown_compiledfns();
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
  setup_scopes();
  setup_compiledfns();
  setup_streams();
  Interactive = false;
  Hide_warnings = false;
  Passed = true;
}



//// helpers for tests

void read_all(string s) {
  stringstream ss(s);
  indent_sensitive_stream in(ss);
  do {
      rmref(read(in));
  } while (!in.eof());
  // return nothing; we'll just verify the trace
}

void run(string s) {
  stringstream in(s);
  rmref(run(in));
  // return nothing; we'll just verify the trace
}

cell* read(string s) {
  return read(*new stringstream(s));
}

string time_string() {
  time_t t;
  time(&t);
  char buffer[10];
  if (!strftime(buffer, 10, "%H:%M:%S", localtime(&t)))
    return "";
  return buffer;
}
