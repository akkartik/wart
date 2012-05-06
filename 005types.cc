//// primitive datatypes

                                  unordered_map<int, Cell*> intLiterals;

Cell* newNum(int x) {
  if (intLiterals[x])
    return intLiterals[x];
  intLiterals[x] = newCell();
  intLiterals[x]->car = (Cell*)x;
  intLiterals[x]->type = NUMBER;
  return mkref(intLiterals[x]);
}

bool isNum(Cell* x) {
  return x->type == NUMBER;
}

int toInt(Cell* x) {
  if (!isNum(x)) {
    RAISE << "not a number: " << x << endl;
    return 0;
  }
  return (int)x->car;
}



                                  template<class Data>
                                  struct StringMap :public unordered_map<string, Data>{};

                                  StringMap<Cell*> symLiterals;

Cell* newSym(string x) {
  if (symLiterals[x])
    return symLiterals[x];
  symLiterals[x] = newCell();
  symLiterals[x]->car = (Cell*)new string(x); // not aligned like cells; can fragment memory
  symLiterals[x]->type = SYMBOL;
  return mkref(symLiterals[x]);
}

bool isSym(Cell* x) {
  return x->type == SYMBOL;
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
  static int counter = 0;
  ostringstream os;
  os << (x == nil ? "sym" : toString(x)) << ++counter;
  return newSym(os.str());
}



unordered_set<Cell*> initialSyms;
void teardownLiteralTables() {
  for (unordered_map<int, Cell*>::iterator p = intLiterals.begin(); p != intLiterals.end(); ++p) {
    if (p->second->nrefs > 1)
      RAISE << "couldn't unintern: " << p->first << ": " << (void*)p->second << " " << (int)p->second->car << " " << p->second->nrefs << endl;
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



Cell* newCompiledFn(CompiledFn f) {
  Cell* result = newCell();
  result->type = COMPILED_FN;
  result->car = (Cell*)f;
  return result;
}

bool isCompiledFn(Cell* x) {
  return x->type == COMPILED_FN;
}

CompiledFn toCompiledFn(Cell* x) {
  if (!isCompiledFn(x))
    RAISE << "Not a compiled function" << endl << DIE;
  return (CompiledFn)x->car;
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

void set(Cell* t, Cell* k, Cell* val) {
  unsafeSet(t, k, val, true);
}

void set(Cell* t, string k, Cell* val) {
  unsafeSet(t, newSym(k), val, true);
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

Cell* get(Cell* t, string k) {
  return get(t, newSym(k));
}



Cell* newObject(string type, Cell* rep) {
  return newCons(newSym("object"), newCons(newSym(type), newCons(rep, nil)));
}

bool isObject(Cell* x) {
  return car(x) == newSym("object");
}

Cell* rep(Cell* x) {
  return car(cdr(cdr(x)));
}

Cell* type(Cell* x) {
  if (x == nil) return nil;
  switch(x->type) {
  case NUMBER:
    return newSym("number");
  case SYMBOL:
    return newSym("symbol");
  case STRING:
    return newSym("string");
  case TABLE:
    return newSym("table");
  case COMPILED_FN:
    return newSym("function");
  case CONS:
    if (isObject(x))
      return car(cdr(x));
    return newSym("list");
  default:
    RAISE << "Undefined type: " << x->type << endl << DIE;
    return nil; // never reached
  }
}

// mkrefs its result
Cell* coerceQuoted(Cell* x, Cell* destType, Cell* coercions) {
  Cell* typ = type(x);
  if (typ == destType)
    return mkref(x);

  if (coercions == nil) RAISE << "coercions* not initialized yet\n";
  if (!isTable(coercions)) RAISE << "coercions* not a table\n";
  Cell* tmp = get(coercions, destType);
  if (tmp == nil) RAISE << "coercions* for " << destType << " not initialized\n";
  if (!isTable(coercions)) RAISE << "coercions* for " << destType << " not a table\n";
  Cell* coercer = get(tmp, typ);
  if (coercer == nil) {
    RAISE << "can't coerce " << typ << " " << x << " to " << destType << endl;
    return nil;
  }
  Cell* expr = newCons(coercer, newCons(newCons(newSym("'"), x), nil));
  Cell* result = eval(expr);
  rmref(expr);
  return result; // already mkref'd
}
