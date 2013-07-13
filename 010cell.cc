//// cell: core lisp data structure with ref-counted garbage collection

// Design considered the following:
//  represent numbers, strings, symbols, lists, and hash-tables
//  reclaim unused cells
//  minimize memory footprint
//  avoid fragmentation
//    so all cells try to have the same size (exceptions: strings, tables)

unsigned long Num_allocs = 0;

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
    #define COMPILED_FN 6
  int nrefs;

  cell() :car(NULL), cdr(NULL), type(CONS), nrefs(0) {}
  void init() {
    car=cdr=nil, type=CONS, nrefs=0;
    ++Num_allocs;
  }
  void clear() { car=cdr=NULL, type=CONS, nrefs=0; }
};

cell* nil = new cell;
void setup_nil() {
  nil->car = nil->cdr = nil;
}

bool is_cons(cell* x) {
  return x != nil && x->type == CONS;
}

bool is_atom(cell* x) {
  return x == nil || x->type == INTEGER || x->type == FLOAT || x->type == STRING || x->type == SYMBOL || x->type == COMPILED_FN;
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
  trace("gc") << "grow_heap";
  Curr_heap = Curr_heap->next = new heap();
  if (!Curr_heap) {
    RAISE << "Out of memory\n" << die();
    exit(0);
  }
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
  trace("gc") << "alloc";
  cell* result = NULL;
  if (Free_cells) {
    trace("gc/alloc") << "reuse";
    result = Free_cells;
    Free_cells = Free_cells->cdr;
  }
  else {
    trace("gc/alloc") << "new";
    if (Curr_cell == CELLS_PER_HEAP)
      grow_heap();

    result = &Curr_heap->cells[Curr_cell];
    ++Curr_cell;
  }
  result->init();
  trace("gcdump") << "alloc: " << (void*)result;
  return result;
}

void free_cell(cell* c) {
  trace("gc") << "free";
  c->clear();
  c->cdr = Free_cells;
  Free_cells = c;
  return;
}



typedef unordered_map<cell*, cell*> cell_map;

struct table {
  cell_map value;
  cell*& operator[](cell* c) {
    return value[c];
  }

  ~table() {
    for (cell_map::iterator p = value.begin(); p != value.end(); ++p) {
      if (!p->second) continue;
      rmref(p->first);
      rmref(p->second);
    }
  }
};

cell* mkref(cell* c) {
  if (c == nil) return nil;
  trace("gc") << "mkref";
  trace("gc/mkref") << c;
  ++c->nrefs;
  return c;
}

void rmref(cell* c) {
  if (!c) {
    RAISE << "A cell was prematurely garbage-collected.\n" << die();
    return;
  }
  if (c == nil) return;

  new_trace_frame("rmref");
  trace("gc") << "rmref";
  trace("gc/rmref") << c;

  if (c->nrefs <= 0) cerr << 'X' << c << '\n';
  cerr.flush();
  --c->nrefs;
  if (c->nrefs > 0) return;

  trace("gcdump") << "free: " << (void*)c << " " << c << " " << c->nrefs;
  if (c->type == INTEGER || c->type == SYMBOL)
    RAISE << "deleted interned atom: " << c << '\n';

  switch (c->type) {
  case INTEGER:
  case FLOAT:
    break;  // numbers don't need freeing
  case STRING:
  case SYMBOL:
    delete (string*)c->car;  break;
  case CONS:
    rmref(c->car);  break;
  case TABLE:
    delete (table*)c->car;  break;
  case COMPILED_FN:
    break;  // compiled functions don't need freeing
  default:
    RAISE << "Can't rmref type " << c->type << '\n' << die();
    return;
  }

  rmref(c->cdr);
  free_cell(c);
}

// helper for tests
bool is_free(cell* x) {
  return x->car == NULL;
}

long excess_mkrefs() {
  return trace_count("gc", "mkref") - trace_count("gc", "rmref");
}



//// tracking refcounts

// RAII for temporaries
struct lease_cell {
  cell*& value;  // reference allows us to track changes to the underlying temporary
  lease_cell(cell*& v) :value(v) {}
  ~lease_cell() {
    trace("gc/out of scope") << value;
    rmref(value);
  }
};

#define TEMP(var, cell_expr)  cell* var = cell_expr;  lease_cell lease_##var(var);

void update(cell*& var, cell* expr) {
  rmref(var);
  var = expr;
}



//// Debugging leaks.

long num_unfreed() {
  long n = 0;
  for (heap* h = First_heap; h != Curr_heap; h=h->next)
    n += CELLS_PER_HEAP;
  n += Curr_cell;
  for (cell* f = Free_cells; f; f=f->cdr) {
    --n;
    if (n < 0) {
      RAISE << "Non-null pointer in reclaimed cell. It was probably prematurely reclaimed.\n" << die();
      break;
    }
  }
  return n;
}

void dump_unfreed() {
  unordered_map<cell*, long> num_refs_remaining;
  for (heap* h = First_heap; h; h=h->next)
    for (cell* x = &h->cells[0]; x < &h->cells[CELLS_PER_HEAP]; ++x)
      if (x->car)
        mark_all_cells(x, num_refs_remaining);

  for (heap* h = First_heap; h; h=h->next)
    for (cell* x = &h->cells[0]; x < &h->cells[CELLS_PER_HEAP]; ++x) {
      if (!x->car) continue;
      if (num_refs_remaining[x] > 1) continue;
      if (is_sym(x) && to_string(x) == "Curr_lexical_scope")
        continue;
      cerr << "unfreed: " << (void*)x << " " << x << '\n';
    }
}

void mark_all_cells(cell* x, unordered_map<cell*, long>& mark) {
  if (x == nil) return;
  ++mark[x];
  switch (x->type) {
  case INTEGER:
  case FLOAT:
  case SYMBOL:
  case STRING:
    break;
  case CONS:
    mark_all_cells(car(x), mark);  break;
  case TABLE: {
    table* t = (table*)x->car;
    for (cell_map::iterator p = t->value.begin(); p != t->value.end(); ++p) {
      if (!p->second) continue;
      mark_all_cells(p->first, mark);
      mark_all_cells(p->second, mark);
    }
    break;
  }
  case COMPILED_FN:
    break;
  default:
    cerr << "Can't mark type " << x->type << '\n' << die();
    return;
  }
  mark_all_cells(cdr(x), mark);
}
