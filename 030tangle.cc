void tangle_and_print_all(const int argc, const char* argv[]) {
  list<string> result;
  for (int n = 0; n < argc; ++n) {
    ifstream in(argv[n]);
    tangle(in, result);
  }
  for (list<string>::iterator p = result.begin(); p != result.end(); ++p)
    cout << *p << '\n';
}

void tangle(istream& in, list<string>& out) {
  string curr_line;
  while (!in.eof()) {
    getline(in, curr_line);
    if (starts_with(curr_line, ":("))
      process_next_hunk(in, trim(curr_line), out);
    else
      out.push_back(curr_line);
  }
  trace_all("tangle", out);
}

void process_next_hunk(istream& in, const string& directive, list<string>& out) {
  list<string> hunk;
  string curr_line;
  while (!in.eof()) {
    std::streampos old = in.tellg();
    getline(in, curr_line);
    if (starts_with(curr_line, ":(")) {
      in.seekg(old);
      break;
    }
    else {
      hunk.push_back(curr_line);
    }
  }

  TEMP(expr, read(directive.substr(1)));
  cell* x1 = car(cdr(expr));
  list<string>::iterator target = (x1 != nil) ? find_substr(out, to_string(x1)) : out.end();
  string cmd = to_string(car(expr));
  if (cmd == "after") {
    if (target == out.end()) RAISE << "couldn't find target " << to_string(car(cdr(expr))) << '\n';
    ++target;
  }
  out.insert(target, hunk.begin(), hunk.end());
}

list<string>::iterator find_substr(list<string>& in, const string& pat) {
  for (list<string>::iterator p = in.begin(); p != in.end(); ++p)
    if (p->find(pat) != NOT_FOUND)
      return p;
  return in.end();
}

#include <locale>
using std::isspace;   // unicode-aware

// does s start with pat, after skipping whitespace?
// pat can't start with whitespace
bool starts_with(const string& s, const string& pat) {
  for (size_t pos = 0; pos < s.size(); ++pos)
    if (!isspace(s[pos]))
      return s.compare(pos, pat.size(), pat) == 0;
  return false;
}

size_t indent(const string& s) {
  for (size_t pos = 0; pos < s.size(); ++pos)
    if (!isspace(s[pos]))
      return pos;
  return 0;
}

string strip_indent(const string& s, size_t n) {
  if (s.empty()) return "";
  string::const_iterator curr = s.begin();
  while (curr != s.end() && n > 0 && isspace(*curr)) {
    ++curr;
    --n;
  }
  return string(curr, s.end());
}

string trim(const string& s) {
  string::const_iterator first = s.begin();
  while (first != s.end() && isspace(*first))
    ++first;
  if (first == s.end()) return "";

  string::const_iterator last = --s.end();
  while (last != s.begin() && isspace(*last))
    --last;
  ++last;
  return string(first, last);
}
