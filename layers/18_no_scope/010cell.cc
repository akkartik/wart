//// cell: core lisp data structure with ref-counted garbage collection

// Design considered the following:
//  represent numbers, strings, symbols, lists, and hash-tables

extern cell* nil;

struct cell {
  cell* car;  // aliased to long or float
  cell* cdr;

  // ints save space on 64-bit platforms
  int type;
    #define CONS 0
    #define INTEGER 1
    #define FLOAT 2
    #define SYMBOL 3
    #define STRING 4
    #define TABLE 5

  cell() :car(NULL), cdr(NULL), type(CONS) {}
  void init() {
    car=cdr=nil, type=CONS;
  }
};

cell* nil = new cell;
void setup_nil() {
  nil->car = nil->cdr = nil;
}

bool is_cons(cell* x) {
  return x != nil && x->type == CONS;
}

bool is_atom(cell* x) {
  return x == nil || x->type == INTEGER || x->type == FLOAT || x->type == STRING || x->type == SYMBOL;
}



#define CELLS_PER_HEAP (4*1024/sizeof(cell))  // default linux pagesize
struct heap {
  cell cells[CELLS_PER_HEAP];
  heap *next;
  heap() :next(NULL) {}
};

heap* First_heap = new heap();
heap* Curr_heap = First_heap;
long Curr_cell = 0;
cell* Free_cells = NULL;

void grow_heap() {
  Curr_heap = Curr_heap->next = new heap();
  if (!Curr_heap) RAISE << "Out of memory\n" << die();
  Curr_cell = 0;
}

void reset_heap(heap* h) {
  if (h->next)
    reset_heap(h->next);
  delete h;
  if (h == First_heap) {
    First_heap = new heap();
    Curr_heap = First_heap;
    Curr_cell = 0;
    Free_cells = NULL;
  }
}

cell* new_cell() {
  cell* result = NULL;
  if (Free_cells) {
    result = Free_cells;
    Free_cells = Free_cells->cdr;
    result->init();
    return result;
  }

  if (Curr_cell == CELLS_PER_HEAP)
    grow_heap();

  result = &Curr_heap->cells[Curr_cell];
  ++Curr_cell;
  result->init();
  return result;
}
