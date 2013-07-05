//// primitive datatypes: lists

cell* car(cell* x) {
  if (x->type != CONS) {
    RAISE << "car of non-cons: " << x << '\n';
    return nil;
  }
  return x->car;
}

cell* cdr(cell* x) {
  return x->cdr;
}

void set_car(cell* x, cell* y) {
  if (x == nil) {
    RAISE << "set_car on nil\n";
    return;
  }
  x->car = y;
}

void set_cdr(cell* x, cell* y) {
  if (x == nil) {
    RAISE << "set_cdr on nil\n";
    return;
  }
  x->cdr = y;
}

cell* new_cons(cell* car, cell* cdr) {
  cell* ans = new_cell();
  set_car(ans, car);
  set_cdr(ans, cdr);
  return ans;
}

cell* new_cons(cell* car) {
  return new_cons(car, nil);
}



//// numbers

unordered_map<long, cell*> Int_literals;

cell* new_num(long x) {
  if (Int_literals[x])
    return Int_literals[x];
  Int_literals[x] = new_cell();
  Int_literals[x]->car = (cell*)x;
  Int_literals[x]->type = INTEGER;
  return Int_literals[x];
}

cell* new_num(int x) {   // just for integer literals
  return new_num((long)x);
}

bool is_num(cell* x) {
  return x->type == INTEGER;
}

long to_int(cell* x) {
  // ignore endianness; cells are never persisted
  if (x->type == INTEGER)
    return (long)x->car;
  RAISE << "not a number: " << x << '\n';
  return 0;
}



//// symbols

unordered_map<string, cell*> Sym_literals;

cell* new_sym(string x) {
  if (Sym_literals[x])
    return Sym_literals[x];
  Sym_literals[x] = new_cell();
  Sym_literals[x]->car = (cell*)new string(x);   // not aligned like cells; can fragment memory
  Sym_literals[x]->type = SYMBOL;
  return Sym_literals[x];
}

bool is_sym(cell* x) {
  return x->type == SYMBOL;
}

cell* new_string(string x) {   // don't intern strings
  cell* result = new_cell();
  result->car = (cell*)new string(x);
  result->type = STRING;
  return result;
}

bool is_string(cell* x) {
  return x->type == STRING;
}

string to_string(cell* x) {
  if (!is_string(x) && !is_sym(x)) {
    RAISE << "can't convert to string: " << x << '\n';
    return "";
  }
  return *(string*)x->car;
}



//// internals

void setup_cells() {
  setup_nil();
  Int_literals.clear();
  Sym_literals.clear();
  reset_heap(First_heap);
}
