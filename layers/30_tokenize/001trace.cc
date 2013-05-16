ostream* tr = NULL;

#define trace if (tr) (*tr)

// tr is a resource, LeaseTracer uses RAII to manage it.
struct LeaseTracer {
  LeaseTracer() { tr = new ostringstream; }
  ~LeaseTracer() { delete tr, tr = NULL; }
};
#define START_TRACING LeaseTracer lease_tracer;

long numFailures = 0;

#define CHECK_EQ(X, Y) if ((X) != (Y)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << " == " << #Y << endl; \
    cerr << "  got " << (X) << endl;  /* BEWARE: multiple eval */ \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

void checkTraceContents(string expected) {
  CHECK_EQ(((ostringstream*)tr)->str(), expected);
}
