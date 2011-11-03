//// cell: core lisp data structure with ref-counted garbage collection

struct Cell {
  Cell* car;
  Cell* cdr;
  long type;
    #define CONS 0
    #define NUM 1
    #define SYM 2
    #define STRING 3
    #define TABLE 4
    #define PRIM_FUNC 5
  long nrefs;
  Cell() :car(nil), cdr(nil), type(CONS), nrefs(0) {}
  void init() { car=cdr=nil, type=CONS, nrefs=0; }
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
  return x == nil || x->type == NUM || x->type == STRING || x->type == SYM || x->type == PRIM_FUNC;
}



#define HEAPCELLS (1024*1024/sizeof(Cell)) // 1MB
struct Heap {
  Cell cells[HEAPCELLS];
  Heap *next;
  Heap() :next(NULL) {}
};

Heap* currHeap = new Heap();
Cell* heapStart = &currHeap->cells[0];
Cell* heapEnd = &currHeap->cells[HEAPCELLS];
Cell* currCell = heapStart;
Cell* freelist = NULL;

void growHeap() {
  currHeap = currHeap->next = new Heap();
  if (!currHeap) err << "Out of memory" << endl << DIE;
  currCell = &currHeap->cells[0];
  heapEnd = &currHeap->cells[HEAPCELLS];
}

Cell* newCell() {
  Cell* result = NULL;
  if (freelist) {
    result = freelist;
    freelist = freelist->cdr;
    result->init();
    dbg << endl << "newCell r: " << (void*)result << " " << result->type << endl;
    return result;
  }

  if (currCell == heapEnd)
    growHeap();

  result = currCell;
  ++currCell;
  dbg << endl << "newCell a: " << (void*)result << " " << result->type << endl;
  return result;
}



                                  struct TypeCastCellHash {
                                    size_t operator()(const Cell* c) const {
                                      hash<long> h;
                                      return h((long)c);
                                    }
                                  };
                                  struct CellMap :public hash_map<Cell*, Cell*, TypeCastCellHash> {};

                                  void rmref(Cell*);

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
  dbg << "mkref: " << (void*)c << " " << c->nrefs << endl;
  ++c->nrefs;
  return c;
}

void rmref(Cell* c) {
  if (!c)
    err << " a cell was prematurely garbage-collected." << endl << DIE;
  if (c == nil) return;
  dbg << endl << "rmref: " << (void*)c << ": " << c->nrefs << " " << c->type << endl;

  --c->nrefs;
  if (c->nrefs > 0) return;

  if (isAtom(c) && c->type != STRING && !runningTests)
    warn << "deleted atom: " << (void*)c << endl;

  switch (c->type) {
  case NUM:
    break; // numbers don't need freeing
  case STRING:
  case SYM:
    dbg << "  delete: " << *(string*)c->car << endl;
    delete (string*)c->car; break;
  case CONS:
    rmref(c->car); break;
  case TABLE:
    dbg << "  delete table" << endl;
    delete (Table*)c->car; break;
  case PRIM_FUNC:
    break; // compiled functions don't need freeing
  default:
    err << "Can't rmref type " << c->type << endl << DIE;
  }

  dbg << "  freeing " << (void*)c << endl;
  rmref(c->cdr);

  c->clear();
  c->cdr = freelist;
  freelist = c;
  return;
}



Cell* car(Cell* x) {
  if (x->type != CONS) {
    warn << "car of non-cons: " << x << endl;
    return nil;
  }
  return x->car;
}

Cell* cdr(Cell* x) {
  return x->cdr;
}

void setCar(Cell* x, Cell* y) {
  if (x == nil) {
    warn << "setCar on nil" << endl;
    return;
  }
  mkref(y);
  if (isCons(x))
    rmref(car(x));
  x->car = y;
}

void setCdr(Cell* x, Cell* y) {
  if (x == nil) {
    warn << "setCdr on nil" << endl;
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
      warn << "list is too short: " << x << " " << n << endl;
    curr=cdr(curr);
  }
  return curr;
}

Cell* last(Cell* x) {
  while(cdr(x) != nil)
    x = cdr(x);
  return x;
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
  setCdr(p, newCons(x, nil));
}
