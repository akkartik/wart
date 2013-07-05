bool Hide_warnings = false;

struct trace_stream {
  vector<pair<string, pair<int, string> > > past_lines;  // [(layer label, frame, line)]
  unordered_map<string, int> frame;
  // accumulator for current line
  ostringstream* curr_stream;
  string curr_layer;
  string dump_layer;
  trace_stream() :curr_stream(NULL) {}
  ~trace_stream() { if (curr_stream) delete curr_stream; }

  ostringstream& stream(string layer) {
    newline();
    curr_stream = new ostringstream;
    curr_layer = layer;
    return *curr_stream;
  }

  // be sure to call this before messing with curr_stream or curr_layer or frame
  void newline() {
    if (!curr_stream) return;
    past_lines.push_back(pair<string, pair<int, string> >(curr_layer, pair<int, string>(frame[curr_layer], curr_stream->str())));
    if (curr_layer == dump_layer || curr_layer == "dump" ||
        (!Hide_warnings && curr_layer == "warn"))
      cerr << frame[curr_layer] << ": " << with_newline(curr_stream->str());
    delete curr_stream;
    curr_stream = NULL;
  }

  string readable_contents(string layer) {  // empty layer = everything
    newline();
    ostringstream output;
    for (vector<pair<string, pair<int, string> > >::iterator p = past_lines.begin(); p != past_lines.end(); ++p)
      if (layer.empty() || prefix_match(layer, p->first))
        output << p->first << "/" << p->second.first << ": " << with_newline(p->second.second);
    return output.str();
  }

  void dump_browseable_contents(string layer) {
    ofstream dump("dump");
    dump << "<div class='frame' frame_index='1'>start</div>\n";
    for (vector<pair<string, pair<int, string> > >::iterator p = past_lines.begin(); p != past_lines.end(); ++p) {
      if (p->first != layer) continue;
      dump << "<div class='frame";
      if (p->second.first > 1) dump << " hidden";
      dump << "' frame_index='" << p->second.first << "'>";
      dump << p->second.second;
      dump << "</div>\n";
    }
    dump.close();
  }

  string with_newline(string s) {
    if (s[s.size()-1] != '\n') return s+'\n';
    return s;
  }
};



trace_stream* Trace_stream = NULL;

#define trace(layer) !Trace_stream ? cerr /*print nothing*/ : Trace_stream->stream(layer)
#define RAISE (!Trace_stream ? cerr /*do print*/ : Trace_stream->stream("warn")) << __FILE__ << ":" << __LINE__ << " "

// RAISE << die exits after printing -- unless Hide_warnings is set.
struct die {};
ostream& operator<<(ostream& os, unused die) {
  if (Hide_warnings) return os;
  os << "dying";
  exit(1);
}

#define CLEAR_TRACE delete Trace_stream, Trace_stream = new trace_stream;

#define DUMP(layer) cerr << Trace_stream->readable_contents(layer)

// Trace_stream is a resource, lease_tracer uses RAII to manage it.
struct lease_tracer {
  lease_tracer() { Trace_stream = new trace_stream; }
  ~lease_tracer() { delete Trace_stream, Trace_stream = NULL; }
};

#define START_TRACING_UNTIL_END_OF_SCOPE lease_tracer leased_tracer;



void trace_all(const string& label, const list<string>& in) {
  for (list<string>::const_iterator p = in.begin(); p != in.end(); ++p)
    trace(label) << *p;
}

bool check_trace_contents(string FUNCTION, string FILE, int LINE, string expected) {
  vector<string> expected_lines = split(expected, "");
  size_t curr_expected_line = 0;
  while (curr_expected_line < expected_lines.size() && expected_lines[curr_expected_line].empty())
    ++curr_expected_line;
  if (curr_expected_line == expected_lines.size()) return true;
  Trace_stream->newline();
  ostringstream output;
  for (vector<pair<string, pair<int, string> > >::iterator p = Trace_stream->past_lines.begin(); p != Trace_stream->past_lines.end(); ++p) {
    vector<string> tmp = split_first(expected_lines[curr_expected_line], ": ");
    string layer = (tmp.size() == 2) ? tmp[0] : "";
    vector<string> layers = split(layer, ",");
    if (!layer.empty() && !any_prefix_match(layers, p->first))
      continue;

    string expected_line = (tmp.size() == 2) ? tmp[1] : tmp[0];
    if (p->second.second != expected_line)
      continue;

    ++curr_expected_line;
    while (curr_expected_line < expected_lines.size() && expected_lines[curr_expected_line].empty())
      ++curr_expected_line;
    if (curr_expected_line == expected_lines.size()) return true;
  }

  ++Num_failures;
  vector<string> tmp = split_first(expected_lines[curr_expected_line], ": ");
  string layer = (tmp.size() == 2) ? tmp[0] : "";
  string expected_line = (tmp.size() == 2) ? tmp[1] : tmp[0];
  cerr << "\nF " << FUNCTION << "(" << FILE << ":" << LINE << "): missing '" << expected_line << "' in trace:\n";
  DUMP(layer);
  Passed = false;
  return false;
}

bool check_trace_contents(string FUNCTION, string FILE, int LINE, string layer, string expected) {  // empty layer == everything, multiple layers, hierarchical layers
  vector<string> expected_lines = split(expected, "");
  size_t curr_expected_line = 0;
  while (curr_expected_line < expected_lines.size() && expected_lines[curr_expected_line].empty())
    ++curr_expected_line;
  if (curr_expected_line == expected_lines.size()) return true;
  Trace_stream->newline();
  ostringstream output;
  vector<string> layers = split(layer, ",");
  for (vector<pair<string, pair<int, string> > >::iterator p = Trace_stream->past_lines.begin(); p != Trace_stream->past_lines.end(); ++p) {
    if (!layer.empty() && !any_prefix_match(layers, p->first))
      continue;
    if (p->second.second != expected_lines[curr_expected_line])
      continue;
    ++curr_expected_line;
    while (curr_expected_line < expected_lines.size() && expected_lines[curr_expected_line].empty())
      ++curr_expected_line;
    if (curr_expected_line == expected_lines.size()) return true;
  }

  ++Num_failures;
  cerr << "\nF " << FUNCTION << "(" << FILE << ":" << LINE << "): missing '" << expected_lines[curr_expected_line] << "' in trace:\n";
  DUMP(layer);
  Passed = false;
  return false;
}

#define CHECK_TRACE_CONTENTS(...) check_trace_contents(__FUNCTION__, __FILE__, __LINE__, __VA_ARGS__)

int trace_count(string layer) {
  return trace_count(layer, "");
}

int trace_count(string layer, string line) {
  Trace_stream->newline();
  long result = 0;
  vector<string> layers = split(layer, ",");
  for (vector<pair<string, pair<int, string> > >::iterator p = Trace_stream->past_lines.begin(); p != Trace_stream->past_lines.end(); ++p) {
    if (any_prefix_match(layers, p->first))
      if (line == "" || p->second.second == line)
        ++result;
  }
  return result;
}

int trace_count(string layer, int frame, string line) {
  Trace_stream->newline();
  long result = 0;
  vector<string> layers = split(layer, ",");
  for (vector<pair<string, pair<int, string> > >::iterator p = Trace_stream->past_lines.begin(); p != Trace_stream->past_lines.end(); ++p) {
    if (any_prefix_match(layers, p->first) && p->second.first == frame)
      if (line == "" || p->second.second == line)
        ++result;
  }
  return result;
}

#define CHECK_TRACE_WARNS()   CHECK(trace_count("warn") > 0)
#define CHECK_TRACE_DOESNT_WARN()   if (trace_count("warn") > 0) { \
  ++Num_failures; \
  cerr << "\nF " << __FUNCTION__ << "(" << __FILE__ << ":" << __LINE__ << "): unexpected warnings\n"; \
  DUMP("warn"); \
  Passed = false; \
  return; \
}

bool trace_doesnt_contain(string layer, string line) {
  return trace_count(layer, line) == 0;
}

bool trace_doesnt_contain(string expected) {
  vector<string> tmp = split(expected, ": ");
  return trace_doesnt_contain(tmp[0], tmp[1]);
}

bool trace_doesnt_contain(string layer, int frame, string line) {
  return trace_count(layer, frame, line) == 0;
}

#define CHECK_TRACE_DOESNT_CONTAIN(...) CHECK(trace_doesnt_contain(__VA_ARGS__))



// manage layer counts in Trace_stream using RAII
struct lease_trace_frame {
  string layer;
  lease_trace_frame(string l) :layer(l) {
    if (!Trace_stream) return;
    Trace_stream->newline();
    ++Trace_stream->frame[layer];
  }
  ~lease_trace_frame() {
    if (!Trace_stream) return;
    Trace_stream->newline();
    --Trace_stream->frame[layer];
  }
};
#define new_trace_frame(layer) lease_trace_frame leased_frame(layer);

bool check_trace_contents(string FUNCTION, string FILE, int LINE, string layer, int frame, string expected) {  // multiple layers, hierarchical layers
  vector<string> expected_lines = split(expected, "");  // hack: doesn't handle newlines in embedded in lines
  size_t curr_expected_line = 0;
  while (curr_expected_line < expected_lines.size() && expected_lines[curr_expected_line].empty())
    ++curr_expected_line;
  if (curr_expected_line == expected_lines.size()) return true;
  Trace_stream->newline();
  ostringstream output;
  vector<string> layers = split(layer, ",");
  for (vector<pair<string, pair<int, string> > >::iterator p = Trace_stream->past_lines.begin(); p != Trace_stream->past_lines.end(); ++p) {
    if (!layer.empty() && !any_prefix_match(layers, p->first))
      continue;
    if (p->second.first != frame)
      continue;
    if (p->second.second != expected_lines[curr_expected_line])
      continue;
    ++curr_expected_line;
    while (curr_expected_line < expected_lines.size() && expected_lines[curr_expected_line].empty())
      ++curr_expected_line;
    if (curr_expected_line == expected_lines.size()) return true;
  }

  ++Num_failures;
  cerr << "\nF " << FUNCTION << "(" << FILE << ":" << LINE << "): missing '" << expected_lines[curr_expected_line] << "' in trace/" << frame << ":\n";
  DUMP(layer);
  Passed = false;
  return false;
}

#define CHECK_TRACE_TOP(layer, expected) CHECK_TRACE_CONTENTS(layer, 1, expected)



const size_t NOT_FOUND = string::npos;

vector<string> split(string s, string delim) {
  vector<string> result;
  string::size_type begin=0, end=s.find(delim);
  while (true) {
    if (end == NOT_FOUND) {
      result.push_back(string(s, begin, NOT_FOUND));
      break;
    }
    result.push_back(string(s, begin, end-begin));
    begin = end+delim.size();
    end = s.find(delim, begin);
  }
  return result;
}

vector<string> split_first(string s, string delim) {
  vector<string> result;
  string::size_type pos=s.find(delim);
  if (pos == NOT_FOUND) {
    result.push_back(s);
  }
  else {
    result.push_back(string(s, 0, pos));
    result.push_back(string(s, pos+delim.size(), NOT_FOUND));
  }
  return result;
}

bool any_prefix_match(const vector<string>& pats, const string& needle) {
  if (pats.empty()) return false;
  if (*pats[0].rbegin() != '/')
    // prefix match not requested
    return find(pats.begin(), pats.end(), needle) != pats.end();
  // first pat ends in a '/'; assume all pats do.
  for (vector<string>::const_iterator p = pats.begin(); p != pats.end(); ++p)
    if (headmatch(needle, *p)) return true;
  return false;
}

bool prefix_match(const string& pat, const string& needle) {
  if (*pat.rbegin() != '/')
    // prefix match not requested
    return pat == needle;
  return headmatch(needle, pat);
}

bool headmatch(const string& s, const string& pat) {
  if (pat.size() > s.size()) return false;
  return std::mismatch(pat.begin(), pat.end(), s.begin()).first == pat.end();
}
