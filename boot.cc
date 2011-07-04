                                  #include<cstdio>
                                  #include<list>
                                  using std::list;
                                  #include<stack>
                                  using std::stack;
                                  #include <ext/hash_map>
                                  using __gnu_cxx::hash_map;
                                  using __gnu_cxx::hash;

                                  #define __unused__ __attribute__((unused))

                                  // use wide strings everywhere

                                  #include<string>
                                  typedef std::wstring string;

                                  #include<iostream>
                                  typedef std::wistream istream;
                                  typedef std::wostream ostream;
                                  #define cin std::wcin
                                  #define cout std::wcout
                                  #define cerr std::wcerr
                                  using std::endl;

                                  #include<sstream>
                                  typedef std::wstringstream stringstream;
                                  typedef std::wostringstream ostringstream;

                                  typedef char ascii;
                                  #define char wchar_t // must come after all system includes

                                  bool debug = false;
                                  #define dbg if(debug) cerr

                                  class Die {};
                                  ostream& operator<<(ostream& os __unused__, Die die __unused__) {
                                    exit(1);
                                  }
                                  Die DIE;

bool runningTests = false;
int numFailures = 0;

#define check(X) if (!(X)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << endl; \
  } \
  else { cerr << "."; fflush(stderr); }

#define check_eq(X, Y) if ((X) != (Y)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << " == " << #Y << endl; \
    cerr << "  got " << (X) << endl; /* BEWARE: multiple eval */ \
  } \
  else { cerr << "."; fflush(stderr); }

stringstream& teststream(string s) {
  stringstream& result = *new stringstream(s);
  result << std::noskipws;
  return result;
}

void checkState();

#include "boot_list"



#define COMPILE_PRIM_FUNC(op, name, params, body) \
  Cell* primFunc_##name() { \
    Cell* result = nil; /* implicit result */ \
    body \
    return result; \
  }

#include "op_list"

struct PrimFuncMetadata {
  string name;
  string params;
  PrimFunc impl;
};

const PrimFuncMetadata primFuncs[] = {
  #include "prim_func_list"
};

void setupPrimFuncs() {
  for (unsigned int i=0; i < sizeof(primFuncs)/sizeof(primFuncs[0]); ++i) {
    Cell* impl = newCell();
    setCar(impl, newPrimFunc(primFuncs[i].impl));
    setCdr(impl, buildCells(parse(parenthesize(tokenize(teststream(L"("+primFuncs[i].params+L")"))))).front());
    newDynamicScope(primFuncs[i].name, impl);
  }
}

void teardownPrimFuncs() {
  for (unsigned int i=0; i < sizeof(primFuncs)/sizeof(primFuncs[0]); ++i)
    endDynamicScope(primFuncs[i].name);
}



                                  //// test harness

                                  typedef void (*testfunc)(void);

                                  const testfunc tests[] = {
                                    #include"test_list"
                                  };

                                  void runTests() {
                                    runningTests = true; // never reset
                                    for (unsigned int i=0; i < sizeof(tests)/sizeof(tests[0]); ++i) {
                                      (*tests[i])();
                                    }
                                    cerr << endl;
                                    if (numFailures == 0) return;

                                    cerr << numFailures << " failure";
                                        if (numFailures > 1) cerr << "s";
                                        cerr << endl;
                                  }

                                  void clearLiteralTables() {
                                    for (hash_map<long, Cell*>::iterator p = numLiterals.begin(); p != numLiterals.end(); ++p) {
                                      if (p->second->nrefs > 1)
                                        cerr << "forcing unintern: " << p->first << ": " << (void*)p->second << " " << (long)p->second->car << " " << p->second->nrefs << endl;
                                      while (p->second->nrefs > 0)
                                        rmref(p->second);
                                    }
                                    numLiterals.clear();
                                    for (StringMap<Cell*>::iterator p = stringLiterals.begin(); p != stringLiterals.end(); ++p) {
                                      if (p->first == L"currLexicalScope") continue; // memory leak
                                      if (p->second->nrefs > 1)
                                        cerr << "forcing unintern: " << p->first << ": " << (void*)p->second << " " << *(string*)p->second->car << " " << p->second->nrefs << endl;
                                      while (p->second->nrefs > 0)
                                        rmref(p->second);
                                    }
                                    stringLiterals.clear();
                                  }

                                  void checkUnfreed() {
                                    int n = currCell-heapStart-1; // ignore empty currLexicalScopes
                                    for (; freelist; freelist = freelist->cdr)
                                      --n;
                                    check_eq(n, 0);
                                  }

                                  void resetState() {
                                    freelist = NULL;
                                    for(Cell* curr=currCell; curr >= heapStart; --curr)
                                      curr->init();
                                    currCell = heapStart;
                                    dynamics.clear(); // leaks memory for strings and tables
                                  }

void setupState() {
  setupNil();
  setupLexicalScope();
  setupPrimFuncs();
}

                                  void checkState() {
                                    teardownPrimFuncs();
                                    clearLiteralTables();
                                    checkUnfreed();

                                    resetState();
                                    setupState();
                                  }



int main(int argc, ascii* argv[]) {
  setupState();

  int pass = 0;
  if (argc > 1) {
    std::string arg1(argv[1]);
    if (arg1 == "test") {
      runTests();
      return 0;
    }
    else if (arg1[0] >= L'0' || arg1[0] <= L'9') {
      pass = atoi(arg1.c_str());
    }
  }

  switch (pass) {
  case 1:
    cout << tokenize(cin); break;
  case 2:
    cout << parenthesize(tokenize(cin)); break;
  case 3:
    cout << parse(parenthesize(tokenize(cin))); break;
  case 4:
  default:
    cout << buildCells(parse(parenthesize(tokenize(cin)))); break;
  }
  return 0;
}

// style:
//  wide unicode strings everywhere
//  no function prototypes
//  indented functions are deemphasized, would be pushed farther down if C permitted
//  no new
//  immutable objects; copy everywhere; no references or pointers except Cell*
