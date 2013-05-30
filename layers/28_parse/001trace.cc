using std::pair;

struct trace_stream {
  vector<pair<string, pair<int, string> > > past_lines;   // [(layer label, level, line)]
  unordered_map<string, int> level;
  // accumulator for current line
  ostringstream* curr_stream;
  string curr_layer;
  trace_stream() :curr_stream(NULL) {}
  ~trace_stream() { if (curr_stream) delete curr_stream; }

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

  string contents(string layer) {
    reset();
    ostringstream output;
    for (vector<pair<string, pair<int, string> > >::iterator p = past_lines.begin(); p != past_lines.end(); ++p)
      if (layer.empty() || p->first == layer)
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
    delete curr_stream;
    curr_stream = NULL;
  }
};

trace_stream* Trace_stream = NULL;

#define trace(layer) !Trace_stream ? cerr : Trace_stream->stream(layer)

#define TRACE_AND_RETURN(layer, X) \
  return (trace(layer) << X << '\n'), X;



// Trace_stream is a resource, lease_tracer uses RAII to manage it.
struct lease_tracer {
  lease_tracer() { Trace_stream = new trace_stream; }
  ~lease_tracer() { delete Trace_stream, Trace_stream = NULL; }
};
#define START_TRACING_UNTIL_END_OF_SCOPE lease_tracer leased_tracer;



// TODO: logically belongs in main.cc with the rest of the test harness
long Num_failures = 0;
#define CHECK_EQ(X, Y) if ((X) != (Y)) { \
    ++Num_failures; \
    cerr << "\nF " << __FUNCTION__ << ": " << #X << " == " << #Y << '\n'; \
    cerr << "  got " << (X) << '\n';  /* BEWARE: multiple eval */ \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

#define check_trace_contents(layer, expected) \
  CHECK_EQ(Trace_stream->contents(layer), expected);



// manage layer counts in Trace_stream using RAII
struct lease_trace_frame {
  string layer;
  lease_trace_frame(string l) :layer(l) {
    Trace_stream->reset();
    ++Trace_stream->level[layer];
  }
  ~lease_trace_frame() {
    Trace_stream->reset();
    --Trace_stream->level[layer];
  }
};
#define new_trace_frame(layer) lease_trace_frame lease_trace_level(layer);

#define check_trace_contents2(layer, level, expected) \
  CHECK_EQ(Trace_stream->contents(layer, level), expected);
