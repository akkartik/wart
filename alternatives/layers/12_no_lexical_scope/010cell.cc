//// cell: core lisp data structure with ref-counted garbage collection

// Design considered the following:
//  represent numbers, strings, symbols, lists, and hash-tables
//  reclaim unused cells
//  minimize memory footprint
//  avoid fragmentation
//    so all cells try to have the same size (exceptions: strings, tables)

unsigned long numAllocs = 0;

extern Cell* nil;

struct Cell {
  Cell* car;  // aliased to long or float
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



#define HEAPCELLS (1024*1024/sizeof(Cell))  // 1MB
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
      rmref(p->first);
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
    RAISE << "A cell was prematurely garbage-collected." << endl << DIE;
  if (c == nil) return;

  --c->nrefs;
  if (c->nrefs > 0) return;

  if (isAtom(c) && c->type != STRING && c->type != FLOAT && !runningTests)
    RAISE << "deleted atom: " << (void*)c << endl;

  switch (c->type) {
  case INTEGER:
  case FLOAT:
    break;  // numbers don't need freeing
  case STRING:
  case SYMBOL:
    delete (string*)c->car; break;
  case CONS:
    rmref(c->car); break;
  case TABLE:
    delete (Table*)c->car; break;
  case COMPILED_FN:
    break;  // compiled functions don't need freeing
  default:
    RAISE << "Can't rmref type " << c->type << endl << DIE;
  }

  rmref(c->cdr);

  c->clear();
  c->cdr = freelist;
  freelist = c;
  return;
}



// misc

long numUnfreed() {
  long n = 0;
  for (Heap* h = firstHeap; h != currHeap; h=h->next)
    n += HEAPCELLS;
  n += currCell;
  for (Cell* f = freelist; f; f=f->cdr)
    --n;
  return n;
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
      if (numRefsRemaining[x] > 1) continue;
      cerr << "unfreed: " << (void*)x << " " << x << endl;
    }
}

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
      markAllCells(p->first, mark);
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
