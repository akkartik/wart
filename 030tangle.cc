list<string> lines(istream& in) {
  list<string> result;
  while (!in.eof()) {
    string s;
    getline(in, s);
    result.push_back(s);
  }
  return result;
}

struct hunk {
  list<string> lines;
};

list<hunk> hunks(istream& in) {
  list<hunk> result;
  list<string> all_lines = lines(in);
  size_t curr_indent = 0;
  trace("tangle") << "first hunk";
  result.push_back(hunk());
  for (list<string>::iterator p = all_lines.begin(); p != all_lines.end(); ++p) {
    if (starts_with(*p, ":(")) {
      trace("tangle") << "new hunk: " << trim(*p);
      result.push_back(hunk());
      curr_indent = indent(*p);
    }
    string line = strip_indent(*p, curr_indent);
    trace("tangle") << "line: " << line;
    result.back().lines.push_back(line);
  }
  return result;
}

#include <locale>
using std::isspace;   // unicode-aware

// does s start with pat, after skipping whitespace?
// pat can't start with whitespace
bool starts_with(const string& s, const string& pat) {
  for (size_t pos = 0; pos < s.size(); ++pos) {
    if (!isspace(s[pos]))
      return s.compare(pos, pat.size(), pat) == 0;
  }
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
