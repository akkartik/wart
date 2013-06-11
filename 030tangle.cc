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
  trace("tangle") << "first hunk";
  result.push_back(hunk());
  for (list<string>::iterator p = all_lines.begin(); p != all_lines.end(); ++p) {
    trace("tangle") << "line: " << *p;
    if (starts_with(*p, ":(")) {
      trace("tangle") << "new hunk: " << *p;
      result.push_back(hunk());
    }
    result.back().lines.push_back(*p);
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
