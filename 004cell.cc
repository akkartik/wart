//// cell: core lisp data structure with ref-counted garbage collection

unsigned long numAllocs = 0;

extern Cell* nil;

struct Cell {
  Cell* car; // aliased to long or float
  Cell* cdr;

  // ints save space on 64-bit platforms
  int type;
    #define CONS 0
    #define INTEGER 1
    #define FLOAT 2
    #define SYMBOL 3
    #define STRING 4
    #define TABLE 5
    #define COMPILED_FN 6
  int nrefs;

  Cell() :car(NULL), cdr(NULL), type(CONS), nrefs(0) {}
  void init() {
    car=cdr=nil, type=CONS, nrefs=0;
    ++numAllocs;
  }
  void clear() { car=cdr=NULL, type=CONS, nrefs=0; }
};

Cell* nil = new Cell;
void setupNil() {
  nil->car = nil->cdr = nil;
}

bool isCons(Cell* x) {
  return x != nil && x->type == CONS;
}

bool isAtom(Cell* x) {
  return x == nil || x->type == INTEGER || x->type == FLOAT || x->type == STRING || x->type == SYMBOL || x->type == COMPILED_FN;
}



#define HEAPCELLS (1024*1024/sizeof(Cell)) // 1MB
struct Heap {
  Cell cells[HEAPCELLS];
  Heap *next;
  Heap() :next(NULL) {}
};

Heap* firstHeap = new Heap();
Heap* currHeap = firstHeap;
long currCell = 0;
Cell* freelist = NULL;

void growHeap() {
  currHeap = currHeap->next = new Heap();
  if (!currHeap) RAISE << "Out of memory" << endl << DIE;
  currCell = 0;
}

void resetHeap(Heap* h) {
  if (h->next)
    resetHeap(h->next);
  delete h;
  if (h == firstHeap) {
    firstHeap = new Heap();
    currHeap = firstHeap;
    currCell = 0;
    freelist = NULL;
  }
}

Cell* newCell() {
  Cell* result = NULL;
  if (freelist) {
    result = freelist;
    freelist = freelist->cdr;
    result->init();
    return result;
  }

  if (currCell == HEAPCELLS)
    growHeap();

  result = &currHeap->cells[currCell];
  ++currCell;
  result->init();
  return result;
}



                                  typedef unordered_map<Cell*, Cell*> CellMap;

struct Table {
  CellMap table;
  Cell*& operator[](Cell* c) {
    return table[c];
  }

  ~Table() {
    for (CellMap::iterator p = table.begin(); p != table.end(); ++p) {
      if (!p->second) continue;
      rmref((Cell*)p->first);
      rmref(p->second);
    }
  }
};

Cell* mkref(Cell* c) {
  if (c == nil) return nil;
  ++c->nrefs;
  return c;
}

void rmref(Cell* c) {
  if (!c)
    RAISE << " a cell was prematurely garbage-collected." << endl << DIE;
  if (c == nil) return;

  --c->nrefs;
  if (c->nrefs > 0) return;

  if (isAtom(c) && c->type != STRING && c->type != FLOAT && !runningTests)
    RAISE << "deleted atom: " << (void*)c << endl;

  switch (c->type) {
  case INTEGER:
  case FLOAT:
    break; // numbers don't need freeing
  case STRING:
  case SYMBOL:
    delete (string*)c->car; break;
  case CONS:
    rmref(c->car); break;
  case TABLE:
    delete (Table*)c->car; break;
  case COMPILED_FN:
    break; // compiled functions don't need freeing
  default:
    RAISE << "Can't rmref type " << c->type << endl << DIE;
  }

  rmref(c->cdr);

  c->clear();
  c->cdr = freelist;
  freelist = c;
  return;
}



Cell* car(Cell* x) {
  if (x->type != CONS) {
    RAISE << "car of non-cons: " << x << endl;
    return nil;
  }
  return x->car;
}

Cell* cdr(Cell* x) {
  return x->cdr;
}

void setCar(Cell* x, Cell* y) {
  if (x == nil) {
    RAISE << "setCar on nil" << endl;
    return;
  }
  mkref(y);
  if (isCons(x))
    rmref(car(x));
  x->car = y;
}

void setCdr(Cell* x, Cell* y) {
  if (x == nil) {
    RAISE << "setCdr on nil" << endl;
    return;
  }
  mkref(y);
  rmref(cdr(x));
  x->cdr = y;
}

Cell* newCons(Cell* car, Cell* cdr) {
  Cell* ans = newCell();
  setCar(ans, car);
  setCdr(ans, cdr);
  return ans;
}

Cell* newCons(Cell* car) {
  return newCons(car, nil);
}



Cell* copyList(Cell* x) {
  if (!isCons(x)) return x;
  return newCons(copyList(car(x)),
      copyList(cdr(x)));
}

bool equalList(Cell* a, Cell* b) {
  if (!isCons(a)) return a == b;
  return equalList(car(a), car(b))
      && equalList(cdr(a), cdr(b));
}

Cell* nthCdr(Cell* x, long n) {
  Cell* curr = x;
  for (long idx = n; idx > 0; --idx) {
    if (!isCons(curr))
      RAISE << "list is too short: " << x << " " << n << endl;
    curr=cdr(curr);
  }
  return curr;
}

void append(Cell* x, Cell* y) {
  while(cdr(x) != nil)
    x = cdr(x);
  setCdr(x, y);
}

// useful idiom: create a dummy cell p, keep appending to it using addCons,
// then return dropPtr(p) which GC's the dummy but mkrefs the rest.
Cell* dropPtr(Cell* p) {
  Cell* x = mkref(cdr(p));
  rmref(p);
  return x;
}

void addCons(Cell* p, Cell* x) {
  setCdr(p, newCons(x));
}

// push p onto l and move one refcount to new head
Cell* pushCons(Cell* p, Cell* l) {
  Cell* result = newCons(p, l);
  rmref(cdr(result));
  return mkref(result);
}

bool contains(Cell* tree, Cell* a, unordered_set<Cell*>& done) {
  // guard against cycles
  if (done.find(tree) != done.end()) return false;
  done.insert(tree);

  if (tree == a) return true;
  if (!isCons(tree)) return false;
  return contains(car(tree), a, done)
         || contains(cdr(tree), a, done);
}

bool contains(Cell* tree, Cell* a) {
  unordered_set<Cell*> done;
  return contains(tree, a, done);
}
