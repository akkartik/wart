using std::pair;

struct trace_stream {
  vector<pair<string, pair<int, string> > > past_lines;   // [(layer label, frame, line)]
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
    if (curr_layer == dump_layer || curr_layer == "dump") cerr << curr_stream->str() << '\n';
    delete curr_stream;
    curr_stream = NULL;
  }

  string readable_contents(string layer) {  // empty layer = everything
    newline();
    ostringstream output;
    for (vector<pair<string, pair<int, string> > >::iterator p = past_lines.begin(); p != past_lines.end(); ++p)
      if (layer.empty())
        output << p->first << "/" << p->second.first << ": " << p->second.second << '\n';
      else if (p->first == layer)
        output                    << p->second.first << ": " << p->second.second << '\n';
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
};



trace_stream* global_trace_stream = NULL;

// global_trace_stream is a resource, lease_tracer uses RAII to manage it.
struct lease_tracer {
  lease_tracer() { global_trace_stream = new trace_stream; }
  ~lease_tracer() { delete global_trace_stream, global_trace_stream = NULL; }
};
#define START_TRACING_UNTIL_END_OF_SCOPE lease_tracer leased_tracer;

#define CLEAR_TRACE delete global_trace_stream, global_trace_stream = new trace_stream;

#define DUMP cerr << global_trace_stream->readable_contents("");



// Main entrypoint.
// never write explicit newlines into trace
#define trace(layer) !global_trace_stream ? cerr /*print nothing*/ : global_trace_stream->stream(layer)

bool check_trace_contents(string FUNCTION, string layer, string expected) {   // empty layer == everything, multiple layers, hierarchical layers
  vector<string> expected_lines = split(expected, '\n');
  size_t curr_expected_line = 0;
  while (curr_expected_line < expected_lines.size() && expected_lines[curr_expected_line].empty())
    ++curr_expected_line;
  if (curr_expected_line == expected_lines.size()) return true;
  global_trace_stream->newline();
  ostringstream output;
  vector<string> layers = split(layer, ',');
  for (vector<pair<string, pair<int, string> > >::iterator p = global_trace_stream->past_lines.begin(); p != global_trace_stream->past_lines.end(); ++p) {
    if (!layer.empty() && !any_prefix_match(layers, p->first))
      continue;
    if (p->second.second != expected_lines[curr_expected_line])
      continue;
    ++curr_expected_line;
    while (curr_expected_line < expected_lines.size() && expected_lines[curr_expected_line].empty())
      ++curr_expected_line;
    if (curr_expected_line == expected_lines.size()) {
      cerr << ".", cerr.flush();
      return true;
    }
  }

  cerr << "\nF " << FUNCTION << ": trace didn't contain " << expected_lines[curr_expected_line] << '\n';
  Passed = false;
  return false;
}

#define CHECK_TRACE_CONTENTS(...) check_trace_contents(__FUNCTION__, __VA_ARGS__)

int trace_count(string layer) {
  return trace_count(layer, "");
}

int trace_count(string layer, string line) {
  long result = 0;
  vector<string> layers = split(layer, ',');
  for (vector<pair<string, pair<int, string> > >::iterator p = global_trace_stream->past_lines.begin(); p != global_trace_stream->past_lines.end(); ++p) {
    if (any_prefix_match(layers, p->first))
      if (line == "" || p->second.second == line)
        ++result;
  }
  return result;
}

int trace_count(string layer, int frame, string line) {
  long result = 0;
  vector<string> layers = split(layer, ',');
  for (vector<pair<string, pair<int, string> > >::iterator p = global_trace_stream->past_lines.begin(); p != global_trace_stream->past_lines.end(); ++p) {
    if (any_prefix_match(layers, p->first) && p->second.first == frame)
      if (line == "" || p->second.second == line)
        ++result;
  }
  return result;
}

bool trace_doesnt_contain(string layer, string line) {
  return trace_count(layer, line) == 0;
}

bool trace_doesnt_contain(string layer, int frame, string line) {
  return trace_count(layer, frame, line) == 0;
}

#define CHECK_TRACE_DOESNT_CONTAIN(...) CHECK(trace_doesnt_contain(__VA_ARGS__))



// manage layer counts in global_trace_stream using RAII
struct lease_trace_frame {
  string layer;
  lease_trace_frame(string l) :layer(l) {
    if (!global_trace_stream) return;
    global_trace_stream->newline();
    ++global_trace_stream->frame[layer];
  }
  ~lease_trace_frame() {
    if (!global_trace_stream) return;
    global_trace_stream->newline();
    --global_trace_stream->frame[layer];
  }
};
#define new_trace_frame(layer) lease_trace_frame leased_frame(layer);

bool check_trace_contents(string FUNCTION, string layer, int frame, string expected) {  // multiple layers, hierarchical layers
  vector<string> expected_lines = split(expected, '\n');
  size_t curr_expected_line = 0;
  while (curr_expected_line < expected_lines.size() && expected_lines[curr_expected_line].empty())
    ++curr_expected_line;
  if (curr_expected_line == expected_lines.size()) return true;
  global_trace_stream->newline();
  ostringstream output;
  vector<string> layers = split(layer, ',');
  for (vector<pair<string, pair<int, string> > >::iterator p = global_trace_stream->past_lines.begin(); p != global_trace_stream->past_lines.end(); ++p) {
    if (!layer.empty() && !any_prefix_match(layers, p->first))
      continue;
    if (p->second.first != frame)
      continue;
    if (p->second.second != expected_lines[curr_expected_line])
      continue;
    ++curr_expected_line;
    while (curr_expected_line < expected_lines.size() && expected_lines[curr_expected_line].empty())
      ++curr_expected_line;
    if (curr_expected_line == expected_lines.size()) {
      cerr << ".", cerr.flush();
      return true;
    }
  }

  cerr << "\nF " << FUNCTION << ": trace didn't contain " << expected_lines[curr_expected_line] << '\n';
  Passed = false;
  return false;
}

#define CHECK_TRACE_TOP(layer, expected) CHECK_TRACE_CONTENTS(layer, 1, expected)



const size_t NOT_FOUND = string::npos;

vector<string> split(string s, char delim) {
  vector<string> result;
  string::size_type begin=0, end=s.find(delim);
  while (true) {
    trace("string split") << begin << '-' << end;
    if (end == NOT_FOUND) {
      trace("string split: inserting") << string(s, begin, end);
      result.push_back(string(s, begin, NOT_FOUND));
      break;
    }
    trace("string split: inserting") << string(s, begin, end-begin);
    result.push_back(string(s, begin, end-begin));
    begin = end+1;
    end = s.find(delim, begin);
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

bool headmatch(const string& s, const string& pat) {
  if (pat.size() > s.size()) return false;
  return std::mismatch(pat.begin(), pat.end(), s.begin()).first == pat.end();
}
