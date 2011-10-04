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



// generate traces for debugging

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



// phases of the interpreter

bool runningTests = false;
int numFailures = 0;

stringstream& stream(string s) {
  stringstream& result = *new stringstream(s);
  result << std::noskipws;
  return result;
}

struct Token;
bool interactive = false;
list<Token> tokenize(istream&);
list<Token> parenthesize(list<Token>);
struct AstNode;
list<AstNode> parse(list<Token>);
struct Cell;
extern Cell* nil;
ostream& operator<<(ostream&, Cell*);
list<Cell*> buildCells(list<AstNode>);

list<Cell*> buildFromStream(istream& f) {
  return buildCells(parse(parenthesize(tokenize(f))));
}

list<Cell*> transform(list<Cell*>);

list<Cell*> wartRead(istream& f) {
  return transform(buildFromStream(f));
}

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
  for (unsigned int i=0; i < sizeof(primFuncs)/sizeof(primFuncs[0]); ++i)
    newDynamicScope(primFuncs[i].name,
        newCons(newPrimFunc(primFuncs[i].impl),
            newCons(buildFromStream(stream(primFuncs[i].params)).front(), nil)));
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
    if (initialSyms.find(p->second) != initialSyms.end()) continue;
    if (p->second->nrefs > 1)
      cerr << "couldn't unintern: " << p->first << ": " << (void*)p->second << " " << *(string*)p->second->car << " " << p->second->nrefs << endl;
    if (p->second->nrefs > 0)
      rmref(p->second);
  }
}

                                  void markAllCells(Cell* x, hash_map<Cell*, long, TypeCastCellHash>& mark) {
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
  hash_map<Cell*, long, TypeCastCellHash> numRefsRemaining;
  for (Cell* x = heapStart; x < currCell; ++x) {
    if (!x->car) continue;
    markAllCells(x, numRefsRemaining);
  }
  for (Cell* x = heapStart; x < currCell; ++x) {
    if (!x->car) continue;
    if (initialSyms.find(x) != initialSyms.end()) continue;
    if (numRefsRemaining[x] > 1) continue;
    cerr << "unfreed: " << (void*)x << " " << x << endl;
  }
}

void checkUnfreed() {
  int n = currCell-heapStart-initialSyms.size();
  for (; freelist; freelist = freelist->cdr)
    --n;
  if (n > 0) {
    warn << "Memory leak!\n";
    dumpUnfreed();
  }
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



// test harness

#define check(X) if (!(X)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << endl; \
  } \
  else { cerr << "|"; fflush(stderr); }

#define checkEq(X, Y) if ((X) != (Y)) { \
    ++numFailures; \
    cerr << endl << "F " << __FUNCTION__ << ": " << #X << " == " << #Y << endl; \
    cerr << "  got " << (X) << endl; /* BEWARE: multiple eval */ \
  } \
  else { cerr << "|"; fflush(stderr); }

#include "test_file_list"

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

  interactive = true; // trigger eval on empty lines
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
//  minimal function prototypes
//  immutable objects; copy everywhere; no pointers except Cell*
