using std::pair;

struct TraceStream {
  vector<pair<string, pair<int, string> > > past_lines;   // [(layer label, level, line)]
  unordered_map<string, int> level;
  // accumulator for current line
  ostringstream* curr_stream;
  string curr_layer;
  string dump_layer;
  TraceStream() :curr_stream(NULL) {}
  ~TraceStream() { if (curr_stream) delete curr_stream; }

  ostringstream& stream(string layer) {
    newline();
    curr_stream = new ostringstream;
    curr_layer = layer;
    return *curr_stream;
  }

  void dump_browseable_contents(string layer) {
    ofstream dump("dump");
    dump << "<div class='level' level_index='1'>start</div>\n";
    for (vector<pair<string, pair<int, string> > >::iterator p = past_lines.begin(); p != past_lines.end(); ++p) {
      if (p->first != layer) continue;
      dump << "<div class='level";
      if (p->second.first > 1) dump << " hidden";
      dump << "' level_index='" << p->second.first << "'>";
      dump << p->second.second;
      dump << "</div>\n";
    }
    dump.close();
  }

  string readable_contents(string layer) {
    newline();
    ostringstream output;
    for (vector<pair<string, pair<int, string> > >::iterator p = past_lines.begin(); p != past_lines.end(); ++p)
      if (layer.empty())
        output << p->first << "/" << p->second.first << ": " << p->second.second << '\n';
      else if (p->first == layer)
        output                    << p->second.first << ": " << p->second.second << '\n';
    return output.str();
  }

  // be sure to call this before messing with curr_stream or curr_layer or level
  void newline() {
    if (!curr_stream) return;
    past_lines.push_back(pair<string, pair<int, string> >(curr_layer, pair<int, string>(level[curr_layer], curr_stream->str())));
    if (curr_layer == dump_layer || curr_layer == "dump") cerr << curr_stream->str();
    delete curr_stream;
    curr_stream = NULL;
  }
};



TraceStream* global_trace_stream = NULL;

// global_trace_stream is a resource, LeaseTracer uses RAII to manage it.
struct LeaseTracer {
  LeaseTracer() { global_trace_stream = new TraceStream; }
  ~LeaseTracer() { delete global_trace_stream, global_trace_stream = NULL; }
};
#define START_TRACING_UNTIL_END_OF_SCOPE LeaseTracer lease_tracer;

#define CLEAR_TRACE delete global_trace_stream, global_trace_stream = new TraceStream;



// Main entrypoint.
// never write explicit newlines into trace
#define trace(layer) !global_trace_stream ? cerr /*print nothing*/ : global_trace_stream->stream(layer)

bool checkTraceContents(string FUNCTION, string layer, string expected) {
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
  passed = false;
  return false;
}

#define CHECK_TRACE_CONTENTS(...) checkTraceContents(__FUNCTION__, __VA_ARGS__)



// manage layer counts in global_trace_stream using RAII
struct LeaseTraceLevel {
  string layer;
  LeaseTraceLevel(string l) :layer(l) {
    global_trace_stream->newline();
    ++global_trace_stream->level[layer];
  }
  ~LeaseTraceLevel() {
    global_trace_stream->newline();
    --global_trace_stream->level[layer];
  }
};
#define incTraceForRestOfScope(layer) LeaseTraceLevel lease_trace_level(layer);

bool checkTraceContents(string FUNCTION, string layer, int level, string expected) {
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
    if (p->second.first != level)
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
  passed = false;
  return false;
}



const size_t NOT_FOUND = string::npos;

vector<string> split(string s, char delim) {
  vector<string> result;
  string::size_type begin=0, end=s.find(delim);
  while (true) {
    trace("string split") << begin << '-' << end << '\n';
    if (end == NOT_FOUND) {
      trace("string split: inserting") << string(s, begin, end) << '\n';
      result.push_back(string(s, begin, NOT_FOUND));
      break;
    }
    trace("string split: inserting") << string(s, begin, end-begin) << '\n';
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
