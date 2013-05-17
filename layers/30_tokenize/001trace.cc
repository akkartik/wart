struct TraceStream {
  unordered_map<string, ostringstream*> layer;
  ostringstream& stream(string l) {
    if (!layer[l]) layer[l] = new ostringstream;
    return *layer[l];
  }
};

TraceStream* tr = NULL;

#define trace(layer) if (tr) tr->stream(layer)

// tr is a resource, LeaseTracer uses RAII to manage it.
struct LeaseTracer {
  LeaseTracer() { tr = new TraceStream; }
  ~LeaseTracer() { delete tr, tr = NULL; }
};
#define START_TRACING_UNTIL_END_OF_SCOPE LeaseTracer lease_tracer;

long numFailures = 0;

#define CHECK_EQ(X, Y) if ((X) != (Y)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << " == " << #Y << endl; \
    cerr << "  got " << (X) << endl;  /* BEWARE: multiple eval */ \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

void checkTraceContents(string layer, string expected) {
  CHECK_EQ(tr->stream(layer).str(), expected);
}
