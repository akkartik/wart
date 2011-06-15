#include<stdio.h>
#include<string>
using std::string;
#include<list>
using std::list;
#include<iostream>
using std::istream;
#include<sstream>
using std::stringstream;

int numFailures = 0;

#define check(X) if (!(X)) { ++numFailures; fprintf(stderr, "F %s: %s\n", __FUNCTION__, #X); } \
  else { fprintf(stderr, "."); fflush(stderr); }



//// insert explicit parens

enum ParenTokenType {
  TOKEN,
  START_OF_LINE,
  INDENT,
  OUTDENT,
  STRING,
};

struct ParenToken {
  ParenTokenType code;
  string token;
  ParenToken(string x) :token(x) {}
  ParenToken(ParenTokenType x) :code(x) {}
};

list<ParenToken> parseParens(istream& in) {
  static ParenToken prev(START_OF_LINE);
  list<ParenToken> result;
  return result;
}

void test_emptyInput() {
  stringstream ss("");
  check(parseParens(ss).empty());
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
