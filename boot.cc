#include<cstdio>
#include<cstring>
#include<cstdlib>
#include<time.h>
#include<vector>
using std::vector;
#include<list>
using std::list;
#include<stack>
using std::stack;
#include<tr1/unordered_map>
using std::tr1::unordered_map;
#include<tr1/unordered_set>
using std::tr1::unordered_set;
#include<algorithm>

#include<string>
using std::string;

#include<iostream>
using std::istream;
using std::ostream;
using std::iostream;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

#include<sstream>
using std::stringstream;
using std::ostringstream;

#include<fstream>
using std::ifstream;
using std::ofstream;

bool runningTests = false;
int numFailures = 0;
bool inTest = false;
int raiseCount = 0;



// generate traces for debugging

int debug = 0;
#define dbg if(debug == 1) cerr
#define dbg2 if(debug == 2) cerr

#define unused __attribute__((unused))

struct Die {};
ostream& operator<<(unused ostream& os, unused Die die) {
  os << "dying";
  exit(1);
}
Die DIE;

                     // ?: to avoid dangling-else warnings
#define RAISE inTest ? ++raiseCount,cout \
                     : cerr << __FILE__ << ":" << __LINE__ << " "



// interpreter decls

stringstream& stream(string s) {
  stringstream& result = *new stringstream(s);
  result << std::noskipws;
  return result;
}

struct Cell;
extern Cell* nil;
ostream& operator<<(ostream&, Cell*);
struct CodeStream;
Cell* read(CodeStream& c);
bool interactive = false; // eval on multiple newlines
Cell* eval(Cell*);



// pre-compiled primitives

#define COMPILE_PRIM_FUNC(op, name, params, body) \
  Cell* name() { body } /* we extract op and params into prim_func_list */

typedef Cell* (*PrimFunc)();

#include "file_list" // rest of the interpreter

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
    Cell* f = newTable();
    unsafeSet(f, newSym("sig"), nextRawCell(stream(primFuncs[i].params)), false);
    unsafeSet(f, newSym("body"), newPrimFunc(primFuncs[i].impl), false);
    newDynamicScope(primFuncs[i].name, newObject("function", f));
  }
}

void teardownPrimFuncs() {
  for (unsigned int i=0; i < sizeof(primFuncs)/sizeof(primFuncs[0]); ++i)
    endDynamicScope(primFuncs[i].name);
}



// transform code before eval (ssyntax, etc.)

typedef Cell* (*transformer)(Cell*);
const transformer transforms[] = {
  #include "transform_list"
};

Cell* transform(Cell* cell) {
  for (unsigned int i=0; i < sizeof(transforms)/sizeof(transforms[0]); ++i)
    cell = (*transforms[i])(cell);
  return cell;
}

Cell* read(CodeStream& c) {
  Cell* mkref(Cell*);
  return mkref(transform(nextRawCell(c)));
}

void init() {
  numLiterals.clear();
  stringLiterals.clear();
  dynamics.clear(); // leaks memory for strings and tables
  resetHeap();

  setupNil();
  setupLexicalScope();
  setupStreams();
  setupPrimFuncs();
  raiseCount = 0;
}



// check for leaks in tests

                                  void markAllCells(Cell* x, unordered_map<Cell*, long>& mark) {
                                    if (x == nil) return;
                                    ++mark[x];
                                    switch (x->type) {
                                    case NUM:
                                    case STRING:
                                    case SYM:
                                      break;
                                    case CONS:
                                      markAllCells(car(x), mark); break;
                                    case TABLE: {
                                      Table* t = (Table*)x->car;
                                      for (CellMap::iterator p = t->table.begin(); p != t->table.end(); ++p) {
                                        if (!p->second) continue;
                                        markAllCells((Cell*)p->first, mark);
                                        markAllCells(p->second, mark);
                                      }
                                      break;
                                    }
                                    case PRIM_FUNC:
                                      break;
                                    default:
                                      cerr << "Can't mark type " << x->type << endl << DIE;
                                    }
                                    markAllCells(cdr(x), mark);
                                  }

void dumpUnfreed() {
  unordered_map<Cell*, long> numRefsRemaining;
  for (Heap* h = firstHeap; h; h=h->next)
    for (Cell* x = &h->cells[0]; x < &h->cells[HEAPCELLS]; ++x)
      if (x->car)
        markAllCells(x, numRefsRemaining);

  for (Heap* h = firstHeap; h; h=h->next)
    for (Cell* x = &h->cells[0]; x < &h->cells[HEAPCELLS]; ++x) {
      if (!x->car) continue;
      if (initialSyms.find(x) != initialSyms.end()) continue;
      if (numRefsRemaining[x] > 1) continue;
      cerr << "unfreed: " << (void*)x << " " << x << endl;
    }
}

long numUnfreed() {
  long n = 0;
  for (Heap* h = firstHeap; h != currHeap; h=h->next)
    n += HEAPCELLS;
  n += currCell-initialSyms.size();
  for (Cell* f = freelist; f; f=f->cdr)
    --n;
  return n;
}

void checkForLeaks() {
  teardownStreams();
  teardownPrimFuncs();
  teardownLiteralTables();

  if (numUnfreed() > 0) {
    RAISE << "Memory leak!\n";
    dumpUnfreed();
  }
}



// test harness

#define check(X) if (!(X)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << endl; \
  } \
  else { cerr << "."; fflush(stderr); }

#define checkEq(X, Y) if ((X) != (Y)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << " == " << #Y << endl; \
    cerr << "  got " << (X) << endl; /* BEWARE: multiple eval */ \
  } \
  else { cerr << "."; fflush(stderr); }

#include "test_file_list"

typedef void (*testfunc)(void);
const testfunc tests[] = {
  #include "test_list"
};

void runTests() {
  runningTests = inTest = true; // never reset
  for (unsigned int i=0; i < sizeof(tests)/sizeof(tests[0]); ++i) {
    init();
    (*tests[i])();
    if (raiseCount != 0) cerr << raiseCount << " errors encountered" << endl;
    checkForLeaks();
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



int main(int argc, unused char* argv[]) {
  if (argc > 1) {
    runTests();
    return 0;
  }

  init();
  loadFiles(".wart");

  interactive = true; // trigger eval on empty lines
  catchCtrlC();

  CodeStream cs(cin);
  while (true) {
    cout << numUnfreed() << " " << numAllocs << " " << "wart> ";
    Cell* form = read(cs);
    if (cin.eof()) break;
    Cell* result = eval(form);
    cout << result << endl;
    rmref(result);
    rmref(form);
    reset(cin);
  }
  return 0;
}

// style:
//  wide unicode strings everywhere
//  minimal function prototypes
//  immutable objects; copy everywhere; no pointers except Cell*
