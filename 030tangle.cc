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
  string filename;
  int starting_line;
  list<string> lines;
  hunk() :starting_line(0) {}
};

list<hunk> hunks(istream& in) {
  list<hunk> result;
  list<string> all_lines = lines(in);
  result.push_back(hunk());
  result.back().starting_line = 1;
  for (list<string>::iterator p = all_lines.begin(); p != all_lines.end(); ++p)
    trace("tangle") << "hunk: " << *p;
  result.back().lines.swap(all_lines);
  return result;
}

#include <locale>
using std::isspace;   // unicode-aware

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
