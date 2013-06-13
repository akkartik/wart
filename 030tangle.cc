list<string> tangle(istream& in) {
  list<string> result;
  string curr_line;
  while (!in.eof()) {
    getline(in, curr_line);
    if (starts_with(curr_line, ":("))
      process_next_hunk(in, curr_line, result);
    else
      result.push_back(curr_line);
  }
  return result;
}

void process_next_hunk(istream& in, const string& directive, list<string>& out) {
  list<string> hunk;
  string curr_line;
  while (!in.eof()) {
    std::streampos old = in.tellg();
    cerr << "peek: " << in.peek() << '\n';
    getline(in, curr_line);
    if (starts_with(curr_line, ":(")) {
      in.seekg(old);
//?       put_back(in, curr_line);
      cerr << "after: " << in.peek() << '\n';
      break;
    }
  }
}

void put_back(istream& in, const string& line) {
  cerr << "Attempting to put back " << line << "$\n";
  in.putback('\n');
  for (string::const_reverse_iterator p = line.rbegin(); p != line.rend(); ++p)
    in.putback(*p);
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
