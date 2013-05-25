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
    unordered_set<string> layers = split(layer);
    for (vector<pair<string, pair<int, string> > >::iterator p = past_lines.begin(); p != past_lines.end(); ++p)
      if (layer.empty() || layers.find(p->first) != layers.end())
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

#define trace(layer) !global_trace_stream ? cerr : global_trace_stream->stream(layer)

#define TRACE_AND_RETURN(layer, X) \
  return (trace(layer) << X << '\n'), X;



// global_trace_stream is a resource, LeaseTracer uses RAII to manage it.
struct LeaseTracer {
  LeaseTracer() { global_trace_stream = new TraceStream; }
  ~LeaseTracer() { delete global_trace_stream, global_trace_stream = NULL; }
};
#define START_TRACING_UNTIL_END_OF_SCOPE LeaseTracer lease_tracer;



// TODO: logically belongs in main.cc with the rest of the test harness
long numFailures = 0;
#define CHECK_EQ(X, Y) if ((X) != (Y)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << " == " << #Y << endl; \
    cerr << "  got " << (X) << endl;  /* BEWARE: multiple eval */ \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

#define checkTraceContents(layer, expected) \
  CHECK_EQ(global_trace_stream->contents(layer), expected);



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

unordered_set<string> split(string s) {
  unordered_set<string> result;
  string::size_type begin=0, end=s.find(',');
  while (true) {
    trace("string split") << begin << '-' << end << '\n';
    result.insert(string(s, begin, end));
    if (end == NOT_FOUND) break;
    begin = end+1;
    end = s.find(',', begin);
  }
  return result;
}
