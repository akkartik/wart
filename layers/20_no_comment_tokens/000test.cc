bool Running_tests = false;

typedef void (*test_fn)(void);

const test_fn Tests[] = {
  #include "test_list"
};

bool Passed = true;

long Num_failures = 0;

#define CHECK(X) if (!(X)) { \
    ++Num_failures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << endl; \
    Passed = false; \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

#define CHECK_EQ(X, Y) if ((X) != (Y)) { \
    ++Num_failures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << " == " << #Y << endl; \
    cerr << "  got " << (X) << endl;  /* BEWARE: multiple eval */ \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }
