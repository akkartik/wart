struct TraceStream {
  unordered_map<string, ostringstream*> layer;
  ostringstream& stream(string l) {
    if (!layer[l]) layer[l] = new ostringstream;
    return *layer[l];
  }
};

TraceStream* global_trace_stream = NULL;

#define trace(layer) if (global_trace_stream) global_trace_stream->stream(layer)

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

void checkTraceContents(string layer, string expected) {
  CHECK_EQ(global_trace_stream->stream(layer).str(), expected);
}
