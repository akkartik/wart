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

void trim(string& s) {
  trace("trim") << '^' << s << '$';
  string::iterator first = s.begin();
  while (first != s.end() && isspace(*first))
    ++first;
  s.erase(s.begin(), first);

  string::reverse_iterator last = s.rbegin();
  while (last != s.rend() && isspace(*last))
    ++last;
  s.erase(last.base(), s.end());
  trace("trim") << "=> ^" << s << '$';
}
