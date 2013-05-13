ostream* tr = NULL;

#define trace if (tr) (*tr)

// tr is a resource, LeaseTracer uses RAII to manage it.
struct LeaseTracer {
  LeaseTracer() { tr = new ostringstream; }
  ~LeaseTracer() { delete tr, tr = NULL; }
};

#define START_TRACING LeaseTracer lease_tracer;
#define TRACE_CONTENTS ((ostringstream*)tr)->str()
