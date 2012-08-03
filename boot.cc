#include<cstdio>
#include<cstring>
#include<cstdlib>
#include<errno.h>
#include<time.h>
#include<math.h>
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



// generate traces for debugging

long debug = 0;
#define dbg if(debug == 1) cerr
#define dbg2 if(debug == 2) cerr

#define unused __attribute__((unused))

bool pretendRaise = false;
long raiseCount = 0;

                           // ?: to avoid dangling-else warnings
#define RAISE pretendRaise ? ++raiseCount,cerr \
                           : cerr << __FILE__ << ":" << __LINE__ << " "

struct Die {};
ostream& operator<<(unused ostream& os, unused Die die) {
  os << "dying";
  exit(1);
}
Die DIE;



// interpreter decls

stringstream& stream(string s) {
  stringstream& result = *new stringstream(s);
  result << std::noskipws;
  return result;
}

bool interactive = false; // eval on multiple newlines

#include "type_list"

#define COMPILE_FN(op, name, params, body) \
  Cell* name() { body } /* we extract op and params into compiled_fn_list */

typedef Cell* (*CompiledFn)();

#include "function_list"

// interpreter impl

#include "file_list"



// pre-compiled primitives

struct CompiledFnMetadata {
  string name;
  string params;
  CompiledFn impl;
};

const CompiledFnMetadata compiledFns[] = {
  #include "compiled_fn_list"
};

void setupCompiledFns() {
  newDynamicScope("compiled", newTable());
  for (unsigned long i=0; i < sizeof(compiledFns)/sizeof(compiledFns[0]); ++i) {
    Cell* f = newTable();
    unsafeSet(f, newSym("name"), newSym(compiledFns[i].name), false);
    unsafeSet(f, newSym("sig"), nextRawCell(stream(compiledFns[i].params)), false);
    unsafeSet(f, newSym("body"), newCompiledFn(compiledFns[i].impl), false);
    Cell* obj = newObject("function", f);
    newDynamicScope(compiledFns[i].name, obj);
    // save to a second, immutable place
    set(lookup("compiled"), compiledFns[i].name, obj);
  }
}

void teardownCompiledFns() {
  for (unsigned long i=0; i < sizeof(compiledFns)/sizeof(compiledFns[0]); ++i)
    endDynamicScope(compiledFns[i].name);
  endDynamicScope("compiled");
}



// transform code before eval (ssyntax, etc.)

typedef Cell* (*transformer)(Cell*);
const transformer transforms[] = {
  #include "transform_list"
};

Cell* transform(Cell* cell) {
  for (unsigned long i=0; i < sizeof(transforms)/sizeof(transforms[0]); ++i)
    cell = (*transforms[i])(cell);
  return cell;
}

Cell* read(CodeStream& c) {
  return mkref(transform(nextRawCell(c)));
}

void init() {
  intLiterals.clear();
  symLiterals.clear();
  dynamics.clear(); // leaks memory for strings and tables
  resetHeap(firstHeap);

  setupNil();
  setupLexicalScope();
  setupStreams();
  setupCompiledFns();
  raiseCount = 0;
}



// check for leaks in tests

                                  void markAllCells(Cell* x, unordered_map<Cell*, int>& mark) {
                                    if (x == nil) return;
                                    ++mark[x];
                                    switch (x->type) {
                                    case INTEGER:
                                    case FLOAT:
                                    case SYMBOL:
                                    case STRING:
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
                                    case COMPILED_FN:
                                      break;
                                    default:
                                      cerr << "Can't mark type " << x->type << endl << DIE;
                                    }
                                    markAllCells(cdr(x), mark);
                                  }

void dumpUnfreed() {
  unordered_map<Cell*, int> numRefsRemaining;
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
  teardownCompiledFns();
  teardownLiteralTables();

  if (numUnfreed() > 0) {
    RAISE << "Memory leak!\n";
    dumpUnfreed();
  }
}



// test harness

bool runningTests = false;
long numFailures = 0;

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

typedef void (*TestFn)(void);
const TestFn tests[] = {
  #include "test_list"
};

void runTests() {
  runningTests = true;
  pretendRaise = true;
  for (unsigned long i=0; i < sizeof(tests)/sizeof(tests[0]); ++i) {
    init();
    (*tests[i])();
    if (raiseCount != 0) cerr << raiseCount << " errors encountered" << endl;
    checkForLeaks();
  }

  pretendRaise = false;
  init();
  loadFiles(".wart"); // after GC tests
  loadFiles(".test");

  cerr << endl;
  if (numFailures == 0) return;
  cerr << numFailures << " failure";
      if (numFailures > 1) cerr << "s";
      cerr << endl;
}



                                  void reset(istream& in) {
                                    in.get();
                                    if (interactive) in.get();
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
//  minimal function prototypes
//  immutable objects; copy everywhere; no pointers except Cell*
//  long is the default integer type (it's always as large as a pointer)
//  use int for Cell nrefs (to save space), and for system libs
