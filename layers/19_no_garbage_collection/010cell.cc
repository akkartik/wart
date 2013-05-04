//// cell: core lisp data structure with ref-counted garbage collection

// Design considered the following:
//  represent numbers, strings, symbols, lists, and hash-tables

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

  Cell() :car(NULL), cdr(NULL), type(CONS) {}
  void init() {
    car=cdr=nil, type=CONS;
  }
};

Cell* nil = new Cell;
void setupNil() {
  nil->car = nil->cdr = nil;
}

bool isCons(Cell* x) {
  return x != nil && x->type == CONS;
}

bool isAtom(Cell* x) {
  return x == nil || x->type == INTEGER || x->type == FLOAT || x->type == STRING || x->type == SYMBOL;
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
