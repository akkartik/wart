#include<stdio.h>
#include<string>

int numFailures = 0;

#define check(X) if (!(X)) { ++numFailures; fprintf(stderr, "F %s: %s\n", __FUNCTION__, #X); } \
  else { fprintf(stderr, "."); fflush(stderr); }



void test1() {
  check(2+1 == 2);
}

void test2() {
  check(1+1 == 2);
}



typedef void (*testfunc)(void);

const testfunc tests[] = {
  #include"test_list"
};

void runTests() {
  for (int i = 0; i < sizeof(tests)/sizeof(tests[0]); ++i) {
    (*tests[i])();
  }
  fprintf(stderr, "\n");
  if (numFailures > 1) {
    fprintf(stderr, "%d failures\n", numFailures);
  }
  else if (numFailures == 1) {
    fprintf(stderr, "1 failure\n");
  }
}

int main(int argc, char* argv[]) {
  if (argc == 1) return 0;
  std::string arg1(argv[1]);
  if (arg1 == "test") {
    runTests();
  }
  return 0;
}
