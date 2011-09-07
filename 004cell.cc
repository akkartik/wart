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
    dbg << endl << "newCell r: " << result << " " << result->type << endl;
    return result;
  }

  if (currCell == heapEnd)
    growHeap();

  result = currCell;
  ++currCell;
  dbg << endl << "newCell a: " << result << " " << result->type << endl;
  return result;
}



                                  extern void rmref(Cell*);

struct Table {
  hash_map<long, Cell*> table;
  Cell*& operator[](Cell* c) {
    return table[(long)c];
  }

  ~Table() {
    for (hash_map<long, Cell*>::iterator p = table.begin(); p != table.end(); ++p) {
      if (!p->second) continue;
      rmref((Cell*)p->first);
      rmref(p->second);
    }
  }
};

Cell* mkref(Cell* c) {
  if (c == nil) return nil;
  dbg << "mkref: " << c << " " << c->nrefs << endl;
  ++c->nrefs;
  return c;
}

void rmref(Cell* c) {
  if (!c)
    err << " a cell was prematurely garbage-collected." << endl << DIE;
  if (c == nil) return;
  dbg << endl << "rmref: " << c << ": " << c->nrefs << " " << c->type << endl;

  --c->nrefs;
  if (c->nrefs > 0) return;

  if (isAtom(c) && c->type != STRING && !runningTests)
    warn << "deleted atom of type " << c->type << endl;

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

  dbg << "  freeing " << c << endl;
  rmref(c->cdr);

  c->clear();
  c->cdr = freelist;
  freelist = c;
  return;
}



Cell* newTable() {
  Cell* result = newCell();
  result->type = TABLE;
  result->car = (Cell*)new Table();
  return result;
}

bool isTable(Cell* x) {
  return x->type == TABLE;
}

Table* toTable(Cell* x) {
  if (!isTable(x)) return NULL;
  return (Table*)x->car;
}

                                  hash_map<long, Cell*> numLiterals;
                                  Cell* intern(long x) {
                                    if (numLiterals[x]) {
                                      dbg << endl << "reuse: " << x << " " << numLiterals[x] << endl;
                                      return numLiterals[x];
                                    }
                                    numLiterals[x] = newCell();
                                    numLiterals[x]->car = (Cell*)x;
                                    numLiterals[x]->type = NUM;
                                    mkref(numLiterals[x]);
                                    dbg << endl << "new: " << x << " " << numLiterals[x] << endl;
                                    return numLiterals[x];
                                  }

Cell* newNum(long x) {
  return intern(x);
}

bool isNum(Cell* x) {
  return x->type == NUM;
}

long toNum(Cell* x) {
  if (!isNum(x)) return 0;
  return (long)x->car;
}

                                  struct strEq {
                                    bool operator() (const string& s1, const string& s2) const {
                                      return s1 == s2;
                                    }
                                  };

                                  struct strHash {
                                    static hash<char*> h;
                                    size_t operator() (const string& in) const {
                                      unsigned long h = 0;
                                      for (const char* s=in.c_str(); *s; ++s)
                                        h = 5 * h + *s;
                                      return size_t(h);
                                    }
                                  };

                                  template<class Data>
                                  class StringMap :public hash_map<string, Data, strHash, strEq>{};


                                  StringMap<Cell*> stringLiterals;
                                  Cell* intern(string x) {
                                    if (stringLiterals[x]) {
                                      dbg << endl << "reuse: " << x << endl;
                                      return stringLiterals[x];
                                    }
                                    stringLiterals[x] = newCell();
                                    dbg << endl << "new: " << x << " " << stringLiterals[x] << endl;
                                    stringLiterals[x]->car = (Cell*)new string(x); // not aligned like cells; can fragment memory
                                    mkref(stringLiterals[x]);
                                    return stringLiterals[x];
                                  }

Cell* newSym(string x) {
  Cell* result = intern(x);
  result->type = SYM;
  return result;
}

bool isSym(Cell* x) {
  return x->type == SYM;
}

Cell* newString(string x) {
  dbg << endl << "new string: " << x << endl;
  Cell* result = newCell();
  result->car = (Cell*)new string(x);
  result->type = STRING;
  return result;
}

bool isString(Cell* x) {
  return x->type == STRING;
}

string toString(Cell* x) {
  if (!isString(x) && !isSym(x))
    return L"";
  return *(string*)x->car;
}

Cell* newPrimFunc(PrimFunc f) {
  Cell* result = newCell();
  result->type = PRIM_FUNC;
  result->car = (Cell*)f;
  return result;
}

bool isPrimFunc(Cell* x) {
  return x->type == PRIM_FUNC;
}

PrimFunc toPrimFunc(Cell* x) {
  if (!isPrimFunc(x))
    err << "Not a compiled function" << endl << DIE;
  return (PrimFunc)x->car;
}



Cell* car(Cell* x) {
  if (x->type != CONS) {
    warn << "car of non-cons, type " << x -> type << endl;
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

                                  void unsafeSet(Cell* t, Cell* key, Cell* val, bool deleteNils) {
                                    if (!isTable(t)) {
                                      warn << "set on a non-table: " << t << endl;
                                      return;
                                    }
                                    Table& table = *(Table*)(t->car);
                                    if (table[key])
                                      rmref(table[key]);
                                    if (deleteNils && val == nil) {
                                      rmref(key);
                                      table[key] = NULL;
                                      return;
                                    }
                                    if (!table[key]) mkref(key);
                                    mkref(val);
                                    table[key] = val;
                                  }

void set(Cell* t, Cell* k, Cell* val) {
  unsafeSet(t, k, val, true);
}

                                  Cell* unsafeGet(Cell* t, Cell* key) {
                                    if (!isTable(t)) {
                                      warn << "get on a non-table" << endl;
                                      return nil;
                                    }
                                    Table& table = *(Table*)(t->car);
                                    return table[key];
                                  }

Cell* get(Cell* t, Cell* k) {
  Cell* result = unsafeGet(t, k);
  if (!result) return nil;
  return result;
}



                                  ostream& operator<<(ostream& os, Cell* c);

                                  ostream& operator<<(ostream& os, Table* t) {
                                    os << "{" << endl;
                                    for (hash_map<long, Cell*>::iterator p = t->table.begin(); p != t->table.end(); ++p) {
                                      if (p->second)
                                        os << "  " << (Cell*)p->first << ": " << p->second << endl;
                                    }
                                    return os << "}";
                                  }

                                  ostream& operator<<(ostream& os, Cell* c) {
                                    if (c == NULL) return os << "NULLNULLNULL";
                                    if (c == nil) return os << "nil";
                                    switch(c->type) {
                                    case CONS:
                                      if (car(c) == newSym(L"'") || car(c) == newSym(L"`") || car(c) == newSym(L",") || car(c) == newSym(L",@"))
                                        return os << car(c) << cdr(c);
                                      os << "(" << car(c);
                                      for (Cell* curr = cdr(c); curr != nil; curr = cdr(curr))
                                        if (isCons(curr))
                                          os << " " << car(curr);
                                        else
                                          os << " . " << curr;
                                      return os << ")";
                                    case NUM:
                                      return os << toNum(c);
                                    case SYM:
                                    case STRING:
                                      return os << toString(c);
                                    case TABLE:
                                      os << (Table*)c->car;
                                      if (cdr(c) != nil)
                                        os << cdr(c);
                                      return os;
                                    case PRIM_FUNC:
                                      return os << "#compiled";
                                    default:
                                      return os << "Can't print type " << c->type << endl << DIE;
                                    }
                                  }

ostream& operator<<(ostream& os, list<Cell*> l) {
  for (list<Cell*>::iterator p = l.begin(); p != l.end(); ++p)
    os << *p;
  return os << endl;
}
