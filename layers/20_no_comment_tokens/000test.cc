bool runningTests = false;

typedef void (*TestFn)(void);

const TestFn tests[] = {
  #include "test_list"
};

bool passed = true;

long numFailures = 0;

#define CHECK(X) if (!(X)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << endl; \
    passed = false; \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

#define CHECK_EQ(X, Y) if ((X) != (Y)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << " == " << #Y << endl; \
    cerr << "  got " << (X) << endl;  /* BEWARE: multiple eval */ \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }
