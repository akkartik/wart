//// primitive datatypes

                                  unordered_map<long, Cell*> numLiterals;
                                  Cell* intern(long x) {
                                    if (numLiterals[x])
                                      return numLiterals[x];
                                    numLiterals[x] = newCell();
                                    numLiterals[x]->car = (Cell*)x;
                                    numLiterals[x]->type = NUM;
                                    mkref(numLiterals[x]);
                                    return numLiterals[x];
                                  }

Cell* newNum(long x) {
  return intern(x);
}

bool isNum(Cell* x) {
  return x->type == NUM;
}

long toNum(Cell* x) {
  if (!isNum(x)) {
    RAISE << "not a number: " << x << endl;
    return 0;
  }
  return (long)x->car;
}



                                  template<class Data>
                                  struct StringMap :public unordered_map<string, Data>{};

                                  StringMap<Cell*> stringLiterals;
                                  Cell* intern(string x) {
                                    if (stringLiterals[x])
                                      return stringLiterals[x];
                                    stringLiterals[x] = newCell();
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

Cell* genSym(Cell* x) {
  static long counter = 0;
  ostringstream os;
  os << (x == nil ? "sym" : toString(x)) << ++counter;
  return newSym(os.str());
}



unordered_set<Cell*> initialSyms;
void teardownLiteralTables() {
  for (unordered_map<long, Cell*>::iterator p = numLiterals.begin(); p != numLiterals.end(); ++p) {
    if (p->second->nrefs > 1)
      RAISE << "couldn't unintern: " << p->first << ": " << (void*)p->second << " " << (long)p->second->car << " " << p->second->nrefs << endl;
    if (p->second->nrefs > 0)
      rmref(p->second);
  }
  for (StringMap<Cell*>::iterator p = stringLiterals.begin(); p != stringLiterals.end(); ++p) {
    if (initialSyms.find(p->second) != initialSyms.end()) continue;
    if (p->second->nrefs > 1)
      RAISE << "couldn't unintern: " << p->first << ": " << (void*)p->second << " " << *(string*)p->second->car << " " << p->second->nrefs << endl;
    if (p->second->nrefs > 0)
      rmref(p->second);
  }
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
    RAISE << "Not a compiled function" << endl << DIE;
  return (PrimFunc)x->car;
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

                                  void unsafeSet(Cell* t, Cell* key, Cell* val, bool deleteNils) {
                                    if (!isTable(t)) {
                                      RAISE << "set on a non-table: " << t << endl;
                                      return;
                                    }
                                    Table& table = *(Table*)(t->car);
                                    if (table[key])
                                      rmref(table[key]);
                                    if (deleteNils && val == nil) {
                                      if (!table[key]) return;
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
                                      RAISE << "get on a non-table" << endl;
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



Cell* newType(string type, Cell* rep) {
  return newCons(newSym("type"), newCons(newSym(type), newCons(rep, nil)));
}

// (type _) is request to compute type of something
// (type _ _) is a type expression and evals to itself
bool isTypeExpr(Cell* x) {
  return car(x) == newSym("type") && cdr(cdr(x)) != nil;
}

Cell* rep(Cell* x) {
  return car(cdr(cdr(x)));
}

Cell* type(Cell* x) {
  if (x == nil) return nil;
  switch(x->type) {
  case NUM:
    return newSym("number");
  case SYM:
    return newSym("symbol");
  case STRING:
    return newSym("string");
  case TABLE:
    return newSym("table");
  case PRIM_FUNC:
    return newSym("function");
  case CONS:
    if (isTypeExpr(x))
      return car(cdr(x));
    return newSym("list");
  default:
    RAISE << "Undefined type: " << x->type << endl << DIE;
    return nil; // never reached
  }
}
