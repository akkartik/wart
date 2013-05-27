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
    reset();
    curr_stream = new ostringstream;
    curr_layer = layer;
    return *curr_stream;
  }

  string contents(string layer, int level) {
    reset();
    if (layer.empty()) return "";
    ostringstream output;
    for (vector<pair<string, pair<int, string> > >::iterator p = past_lines.begin(); p != past_lines.end(); ++p)
      if (p->first == layer && p->second.first == level)
        output << p->second.second;
    return output.str();
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

  string contents(string layer) {
    reset();
    ostringstream output;
    vector<string> layers = split(layer, ',');
    for (vector<pair<string, pair<int, string> > >::iterator p = past_lines.begin(); p != past_lines.end(); ++p)
      if (any_prefix_match(layers, p->first))
        output << p->second.second;
    return output.str();
  }

  string readable_contents(string layer) {
    reset();
    ostringstream output;
    for (vector<pair<string, pair<int, string> > >::iterator p = past_lines.begin(); p != past_lines.end(); ++p)
      if (layer.empty() || p->first == layer)
        output << p->second.first << ": " << p->second.second;
    return output.str();
  }

  // be sure to call this before messing with curr_stream or curr_layer or level
  void reset() {
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



#define trace(layer) !global_trace_stream ? cerr /*print nothing*/ : global_trace_stream->stream(layer)

#define TRACE_AND_RETURN(layer, X) \
  return (trace(layer) << X << '\n'), X;

bool checkTraceContents_sub(string FUNCTION, string layer, string expected) {
  vector<string> expected_lines = split(expected, '\n');
  cerr << expected << ": " << expected_lines.size() << endl;
  if (expected_lines.back() == "") expected_lines.pop_back();
  cerr << expected << ": " << expected_lines.size() << endl;
  if (expected_lines.empty()) return true;
  size_t curr_expected_line = 0;
  global_trace_stream->reset();
  ostringstream output;
  vector<string> layers = split(layer, ',');
  for (vector<pair<string, pair<int, string> > >::iterator p = global_trace_stream->past_lines.begin(); p != global_trace_stream->past_lines.end(); ++p) {
    if (*p->second.second.rbegin() == '\n')
      p->second.second.erase(p->second.second.size()-1);
    cerr << "AA: " << p->second.second << " vs " << expected_lines[curr_expected_line] << "$\n";
    if (layer.empty() || any_prefix_match(layers, p->first)) {
      cerr << p->second.second << " vs " << expected_lines[curr_expected_line] << "$\n";
      if (p->second.second == expected_lines[curr_expected_line]) {
        cerr << "incrementing\n";
        ++curr_expected_line;
        if (curr_expected_line == expected_lines.size()) {
          cerr << ".", cerr.flush();
          return true;
        }
      }
    }
  }

  cerr << "\nF " << FUNCTION << ": trace didn't contain " << expected_lines[curr_expected_line] << '\n';
  passed = false;
  exit(0);
  return false;
}

#define checkTraceContents(X, Y) checkTraceContents_sub(__FUNCTION__, X, Y)



// manage layer counts in global_trace_stream using RAII
struct LeaseTraceLevel {
  string layer;
  LeaseTraceLevel(string l) :layer(l) {
    global_trace_stream->reset();
    ++global_trace_stream->level[layer];
  }
  ~LeaseTraceLevel() {
    global_trace_stream->reset();
    --global_trace_stream->level[layer];
  }
};
#define incTraceForRestOfScope(layer) LeaseTraceLevel lease_trace_level(layer);

#define checkTraceContents2(layer, level, expected) \
  CHECK_EQ(global_trace_stream->contents(layer, level), expected);



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
