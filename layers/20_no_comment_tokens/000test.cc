
typedef void (*test_fn)(void);

const test_fn Tests[] = {
  #include "test_list"
};

bool Passed = true;

long Num_failures = 0;

#define CHECK(X) if (!(X)) { \
    ++Num_failures; \
    cerr << "\nF " << __FUNCTION__ << ": " << #X << '\n'; \
    Passed = false; \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }

#define CHECK_EQ(X, Y) if ((X) != (Y)) { \
    ++Num_failures; \
    cerr << "\nF " << __FUNCTION__ << ": " << #X << " == " << #Y << '\n'; \
    cerr << "  got " << (X) << '\n';  /* BEWARE: multiple eval */ \
    return; \
  } \
  else { cerr << "."; fflush(stderr); }
