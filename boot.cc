#include<cstdio>
#include<cstring>
#include<vector>
using std::vector;
#include<list>
using std::list;
#include<stack>
using std::stack;
#include<ext/hash_map>
using __gnu_cxx::hash_map;
using __gnu_cxx::hash;
#include<ext/hash_set>
using __gnu_cxx::hash_set;
#include<exception>



// unicode strings everywhere

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

#include<fstream>
typedef std::wifstream ifstream;
typedef std::wofstream ofstream;

typedef char ascii;
#define char wchar_t // must come after all system includes



int debug = 0;
#define dbg if(debug == 1) cerr
#define dbg2 if(debug == 2) cerr

#define unused __attribute__((unused))

class Die {};
ostream& operator<<(unused ostream& os, unused Die die) {
  exit(1);
}
Die DIE;

#define warn cerr << __FILE__ << ":" << __LINE__ << " "
#define err cerr << "fatal: " << __FILE__ << ":" << __LINE__ << " "



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

stringstream& stream(string s) {
  stringstream& result = *new stringstream(s);
  result << std::noskipws;
  return result;
}



bool interactive = false;

#define COMPILE_PRIM_FUNC(op, name, body) \
  Cell* name(Cell* args) { body } /* ignore op; we extract it into prim_func_list */

#include "file_list" // remaining cc files in order

struct PrimFuncMetadata {
  string name;
  PrimFunc impl;
};

const PrimFuncMetadata primFuncs[] = {
  #include "prim_func_list"
};

void setupPrimFuncs() {
  for (unsigned int i=0; i < sizeof(primFuncs)/sizeof(primFuncs[0]); ++i)
    newDynamicScope(primFuncs[i].name, newCons(newPrimFunc(primFuncs[i].impl), nil));
}

void teardownPrimFuncs() {
  for (unsigned int i=0; i < sizeof(primFuncs)/sizeof(primFuncs[0]); ++i)
    endDynamicScope(primFuncs[i].name);
}



typedef Cell* (*transformer)(Cell*);
const transformer transforms[] = {
  #include "transform_list"
};

Cell* transform(Cell* cell) {
  for (unsigned int i=0; i < sizeof(transforms)/sizeof(transforms[0]); ++i)
    cell = (*transforms[i])(cell);
  return cell;
}

list<Cell*> transform(list<Cell*> input) {
  list<Cell*> result;
  for (list<Cell*>::iterator p = input.begin(); p != input.end(); ++p)
    result.push_back(transform(*p));
  return result;
}



// post-test teardown

void checkLiteralTables() {
  for (hash_map<long, Cell*>::iterator p = numLiterals.begin(); p != numLiterals.end(); ++p) {
    if (p->second->nrefs > 1)
      cerr << "couldn't unintern: " << p->first << ": " << (void*)p->second << " " << (long)p->second->car << " " << p->second->nrefs << endl;
    if (p->second->nrefs > 0)
      rmref(p->second);
  }
  for (StringMap<Cell*>::iterator p = stringLiterals.begin(); p != stringLiterals.end(); ++p) {
    if (p->first == L"currLexicalScope") continue; // leak
    if (p->second->nrefs > 1)
      cerr << "couldn't unintern: " << p->first << ": " << (void*)p->second << " " << *(string*)p->second->car << " " << p->second->nrefs << endl;
    if (p->second->nrefs > 0)
      rmref(p->second);
  }
}

                                  void mark_all_cells(Cell* x, hash_map<long, long>& mark) {
                                    if (x == nil) return;
                                    ++mark[(long)x];
                                    switch (x->type) {
                                    case NUM:
                                    case STRING:
                                    case SYM:
                                      break;
                                    case CONS:
                                      mark_all_cells(car(x), mark); break;
                                    case TABLE: {
                                      Table* t = (Table*)x->car;
                                      for (hash_map<long, Cell*>::iterator p = t->table.begin(); p != t->table.end(); ++p) {
                                        mark_all_cells((Cell*)p->first, mark);
                                        mark_all_cells(p->second, mark);
                                      }
                                      break;
                                    }
                                    case PRIM_FUNC:
                                      break;
                                    default:
                                      cerr << "Can't mark type " << x->type << endl << DIE;
                                    }
                                    mark_all_cells(cdr(x), mark);
                                  }

void dumpUnfreed() {
  hash_map<long, long> numRefsRemaining;
  for (Cell* x = heapStart; x < currCell; ++x) {
    if (!x->car) continue;
    mark_all_cells(x, numRefsRemaining);
  }
  for (Cell* x = heapStart; x < currCell; ++x) {
    if (!x->car) continue;
    if (x == newSym(L"currLexicalScope")) continue;
    if (numRefsRemaining[(long)x] > 1) continue;
    cerr << "unfreed: " << (void*)x << " " << x << endl;
  }
}

void checkUnfreed() {
  int n = currCell-heapStart-1; // ignore empty currLexicalScopes
  for (; freelist; freelist = freelist->cdr)
    --n;
  check_eq(n, 0);
  if (n > 0) dumpUnfreed();
}

void resetState() {
  numLiterals.clear();
  stringLiterals.clear();
  freelist = NULL;
  for(Cell* curr=currCell; curr >= heapStart; --curr)
    curr->init();
  currCell = heapStart;
  dynamics.clear(); // leaks memory for strings and tables
}

void checkState() {
  teardownPrimFuncs();
  checkLiteralTables();
  checkUnfreed();

  resetState();
}



typedef void (*testfunc)(void);

const testfunc tests[] = {
  #include "test_list"
};

void init();
void runTests() {
  runningTests = true; // never reset
  for (unsigned int i=0; i < sizeof(tests)/sizeof(tests[0]); ++i) {
    init();
    (*tests[i])();
    checkState();
  }

  init();
  loadFiles(".wart"); // after GC tests
  loadFiles(".test");

  cerr << endl;
  if (numFailures == 0) return;
  cerr << numFailures << " failure";
      if (numFailures > 1) cerr << "s";
      cerr << endl;
}

void init() {
  setupNil();
  setupLexicalScope();
  setupPrimFuncs();
}



int main(int argc, unused ascii* argv[]) {
  if (argc > 1) {
    runTests();
    return 0;
  }

  init();
  loadFiles(".wart");

  // no unit tests for interactive repl, so manual QA:
  //   single-word expr should eval on newline
  //   multi-word expr that doesn't start with paren should eval on empty line
  //   expr that starts with paren should eval on close
  interactive = true;

  while (!cin.eof()) {
    cout << "wart> ";
    list<Cell*> form = wartRead(cin);
    if (form.empty()) continue;
    Cell* result = eval(form.front());
    cout << result << endl;
    rmref(result);
  }
  return 0;
}

// style:
//  wide unicode strings everywhere
//  no function prototypes
//  immutable objects; copy everywhere; no pointers except Cell*
