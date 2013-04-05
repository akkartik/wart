//// primitive datatypes: lists

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



//// numbers

unordered_map<long, Cell*> intLiterals;

Cell* newNum(long x) {
  if (intLiterals[x])
    return intLiterals[x];
  intLiterals[x] = newCell();
  intLiterals[x]->car = (Cell*)x;
  intLiterals[x]->type = INTEGER;
  return mkref(intLiterals[x]);
}

Cell* newNum(int x) {   // just for integer literals
  return newNum((long)x);
}

Cell* newNum(float x) {   // don't intern floats
  Cell* result = newCell();
  result->car = *(Cell**)&x;
  result->type = FLOAT;
  return result;
}

Cell* newNum(double x) {
  return newNum((float)x);
}

bool isNum(Cell* x) {
  return x->type == INTEGER || x->type == FLOAT;
}

long toInt(Cell* x) {
  // ignore endianness; Cells are never persisted
  if (x->type == INTEGER)
    return (long)x->car;
  if (x->type == FLOAT)
    return (long)*((float*)&x->car);
  RAISE << "not a number: " << x << endl;
  return 0;
}

float toFloat(Cell* x) {
  if (x->type == INTEGER)
    return (long)x->car;
  if (x->type == FLOAT)
    return *(float*)&x->car;
  RAISE << "not a number: " << x << endl;
  return 0;
}

bool equalFloats(float x, float y) {
  return fabs(x-y) < 1e-6;
}



//// symbols

template<class Data>
struct StringMap :public unordered_map<string, Data>{};

StringMap<Cell*> symLiterals;

Cell* newSym(string x) {
  if (symLiterals[x])
    return symLiterals[x];
  symLiterals[x] = newCell();
  symLiterals[x]->car = (Cell*)new string(x);   // not aligned like cells; can fragment memory
  symLiterals[x]->type = SYMBOL;
  return mkref(symLiterals[x]);
}

bool isSym(Cell* x) {
  return x->type == SYMBOL;
}

Cell* newString(string x) {   // don't intern strings
  Cell* result = newCell();
  result->car = (Cell*)new string(x);
  result->type = STRING;
  return result;
}

bool isString(Cell* x) {
  return x->type == STRING;
}

string toString(Cell* x) {
  if (!isString(x) && !isSym(x)) {
    RAISE << "can't convert to string: " << x << endl;
    return "";
  }
  return *(string*)x->car;
}



//// associative arrays

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

void set(Cell* t, string k, Cell* val) {
  unsafeSet(t, newSym(k), val, true);
}

void set(Cell* t, Cell* k, Cell* val) {
  unsafeSet(t, k, val, true);
}

Cell* get(Cell* t, Cell* k) {
  Cell* result = unsafeGet(t, k);
  if (!result) return nil;
  return result;
}

Cell* get(Cell* t, string k) {
  return get(t, newSym(k));
}

void unsafeSet(Cell* t, Cell* key, Cell* val, bool deleteNils) {
  if (!isTable(t)) {
    RAISE << "set on a non-table: " << t << endl;
    return;
  }

  if (isCons(key) || isTable(key) || isString(key))
    RAISE << "table key " << key << " is mutable. If you modify it all bets are off.\n";

  Table& table = *(Table*)(t->car);
  if (val == nil && deleteNils) {
    if (table[key]) {
      rmref(table[key]);
      table[key] = NULL;
      rmref(key);
    }
    return;
  }

  if (val == table[key]) return;

  if (!table[key]) mkref(key);
  else rmref(table[key]);
  table[key] = mkref(val);
}

void unsafeSet(Cell* t, string k, Cell* val, bool deleteNils) {
  unsafeSet(t, newSym(k), val, deleteNils);
}

Cell* unsafeGet(Cell* t, Cell* key) {
  if (!isTable(t)) {
    RAISE << "get on a non-table" << endl;
    return nil;
  }
  Table& table = *(Table*)(t->car);
  return table[key];
}

Cell* unsafeGet(Cell* t, string k) {
  return unsafeGet(t, newSym(k));
}



//// internals

void setupCells() {
  setupNil();
  intLiterals.clear();
  symLiterals.clear();
  resetHeap(firstHeap);
}

void teardownCells() {
  if (nil->car != nil || nil->cdr != nil)
    RAISE << "nil was corrupted" << endl;

  for (unordered_map<long, Cell*>::iterator p = intLiterals.begin(); p != intLiterals.end(); ++p) {
    if (p->second->nrefs > 1)
      RAISE << "couldn't unintern: " << p->first << ": " << (void*)p->second << " " << (long)p->second->car << " " << p->second->nrefs << endl;
    if (p->second->nrefs > 0)
      rmref(p->second);
  }

  for (StringMap<Cell*>::iterator p = symLiterals.begin(); p != symLiterals.end(); ++p) {
    if (initialSyms.find(p->second) != initialSyms.end()) continue;
    if (p->second->nrefs > 1)
      RAISE << "couldn't unintern: " << p->first << ": " << (void*)p->second << " " << *(string*)p->second->car << " " << p->second->nrefs << endl;
    if (p->second->nrefs > 0)
      rmref(p->second);
  }
}

// optimize lookups of common symbols
Cell *sym_quote, *sym_backquote, *sym_unquote, *sym_splice, *sym_unquoteSplice, *sym_alreadyEvald;
Cell *sym_list, *sym_number, *sym_symbol, *sym_string, *sym_table, *sym_List;
Cell *sym_object, *sym_Coercions, *sym_incomplete_eval;
Cell *sym_function, *sym_name, *sym_sig, *sym_body, *sym_optimized_body, *sym_env, *sym_compiled, *sym_param_alias;
Cell *sym_eval, *sym_caller_scope;
void setupCommonSyms() {
  sym_quote = newSym("'");
  sym_backquote = newSym("`");
  sym_unquote = newSym(",");
  sym_splice = newSym("@");
  sym_unquoteSplice = newSym(",@");
  sym_alreadyEvald = newSym("''");

  sym_list = newSym("list");
  sym_number = newSym("number");
  sym_symbol = newSym("symbol");
  sym_string = newSym("string");
  sym_table = newSym("table");
  sym_List = newSym("List");

  sym_object = newSym("object");
  sym_Coercions = newSym("Coercions");
  sym_incomplete_eval = newSym("incomplete_eval");

  sym_function = newSym("function");
  sym_name = newSym("name");
  sym_sig = newSym("sig");
  sym_body = newSym("body");
  sym_optimized_body = newSym("optimized_body");
  sym_env = newSym("env");
  sym_compiled = newSym("compiled");
  sym_param_alias = newSym("|");

  sym_eval = newSym("eval");
  sym_caller_scope = newSym("caller_scope");
}
