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

string Toplevel = "run";

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
  string cmd = to_string(car(expr));

  if (cmd == "code") {
    out.insert(out.end(), hunk.begin(), hunk.end());
    return;
  }

  if (cmd == "scenarios") {
    Toplevel = to_string(car(cdr(expr)));
    return;
  }

  if (cmd == "scenario") {
    list<string> result;
    string name = to_string(car(cdr(expr)));
    emit_test(name, hunk, result);
    out.insert(out.end(), result.begin(), result.end());
    return;
  }

  if (cmd == "before" || cmd == "after") {
    cell* x1 = car(cdr(expr));
    if (x1 == nil) {
      RAISE << "No target for " << cmd << " directive.\n" << die();
      return;
    }
    list<string>::iterator target = find_substr(out, to_string(x1));
    if (target == out.end()) {
      RAISE << "Couldn't find target " << x1 << '\n' << die();
      return;
    }
    string curr_indent = indent(*target);
    for (list<string>::iterator p = hunk.begin(); p != hunk.end(); ++p)
      p->insert(p->begin(), curr_indent.begin(), curr_indent.end());

    if (cmd == "after") ++target;
    out.insert(target, hunk.begin(), hunk.end());
    return;
  }

  RAISE << "unknown directive " << cmd << '\n';
}

// A scenario is one or more sessions separated by calls to CLEAR_TRACE ('===')
//  A session is one or more lines of input
//  followed by a return value ('=>')
//  followed by one or more lines expected in trace in order ('+')
//  followed by one or more lines trace shouldn't include ('-')
// Remember to update is_input below if you add to this format.
void emit_test(const string& name, list<string>& lines, list<string>& result) {
  result.push_back("void test_"+name+"() {");
  if (any_line_starts_with(lines, "=>"))
    result.push_back("  ostringstream os;");
  while (any_non_input_line(lines)) {
    if (!any_line_starts_with(lines, "=>"))
      emit_session(lines, result);  // simpler version; no need to check result
    else
      emit_result_checking_session(lines, result);
    if (!lines.empty() && lines.front()[0] == '+')
      result.push_back("  CHECK_TRACE_CONTENTS(\""+expected_in_trace(lines)+"\");");
    while (!lines.empty() && lines.front()[0] == '-') {
      result.push_back("  CHECK_TRACE_DOESNT_CONTAIN(\""+expected_not_in_trace(lines.front())+"\");");
      lines.pop_front();
    }
    if (!lines.empty() && lines.front() == "===") {
      result.push_back("  CLEAR_TRACE;");
      lines.pop_front();
    }
  }
  result.push_back("}");

  while (!lines.empty() &&
         (trim(lines.front()).empty() || starts_with(lines.front(), "//")))
    lines.pop_front();
  if (!lines.empty()) {
    cerr << lines.size() << " unprocessed lines in scenario.\n";
    exit(1);
  }
}

void emit_session(list<string>& lines, list<string>& result) {
  result.push_back("  "+Toplevel+"(\""+input_lines(lines)+"\");");
}

void emit_result_checking_session(list<string>& lines, list<string>& result) {
  result.push_back("  os.clear();  os.str(\"\");");
  result.push_back("  os << "+Toplevel+"(\""+input_lines(lines)+"\");");
  if (!lines.empty() && starts_with(lines.front(), "=>")) {
    size_t pos = lines.front().find("=>")+2;  // length of '=>'
    result.push_back("  CHECK_EQ(os.str(), \""+trim(string(lines.front(), pos))+"\");");
    lines.pop_front();
  }
}

bool is_input(const string& line) {
  return line != "===" && line[0] != '+' && line[0] != '-' && !starts_with(line, "=>");
}

string input_lines(list<string>& hunk) {
  string result;
  while (!hunk.empty() && is_input(hunk.front())) {
    result += hunk.front()+"";  // temporary delimiter; replace with escaped newline after escaping other backslashes
    hunk.pop_front();
  }
  return escape(result);
}

string expected_in_trace(list<string>& hunk) {
  string result;
  while (!hunk.empty() && hunk.front()[0] == '+') {
    hunk.front().erase(0, 1);
    result += hunk.front()+"";
    hunk.pop_front();
  }
  return escape(result);
}

string expected_not_in_trace(const string& line) {
  return escape(line.substr(1));
}

list<string>::iterator find_substr(list<string>& in, const string& pat) {
  for (list<string>::iterator p = in.begin(); p != in.end(); ++p)
    if (p->find(pat) != NOT_FOUND)
      return p;
  return in.end();
}

string escape(string s) {
  s = replace_all(s, "\\", "\\\\");
  s = replace_all(s, "\"", "\\\"");
  s = replace_all(s, "", "\\n");
  return s;
}

string replace_all(string s, const string& a, const string& b) {
  for (size_t pos = s.find(a); pos != NOT_FOUND; pos = s.find(a, pos+b.size()))
    s = s.replace(pos, a.size(), b);
  return s;
}

bool any_line_starts_with(const list<string>& lines, const string& pat) {
  for (list<string>::const_iterator p = lines.begin(); p != lines.end(); ++p)
    if (starts_with(*p, pat)) return true;
  return false;
}

bool any_non_input_line(const list<string>& lines) {
  for (list<string>::const_iterator p = lines.begin(); p != lines.end(); ++p)
    if (!is_input(*p)) return true;
  return false;
}

#include <locale>
using std::isspace;  // unicode-aware

// does s start with pat, after skipping whitespace?
// pat can't start with whitespace
bool starts_with(const string& s, const string& pat) {
  for (size_t pos = 0; pos < s.size(); ++pos)
    if (!isspace(s[pos]))
      return s.compare(pos, pat.size(), pat) == 0;
  return false;
}

string indent(const string& s) {
  for (size_t pos = 0; pos < s.size(); ++pos)
    if (!isspace(s[pos]))
      return s.substr(0, pos);
  return "";
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
