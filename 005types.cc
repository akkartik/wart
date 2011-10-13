//// primitive datatypes

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
  if (!isNum(x)) {
    warn << "not a number: " << x << endl;
    return 0;
  }
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
  if (!isString(x) && !isSym(x)) {
    warn << "can't convert to string: " << x << endl;
    return L"";
  }
  return *(string*)x->car;
}

Cell* genSym(Cell* x) {
  static long counter = 0;
  ostringstream os;
  os << (x == nil ? L"sym" : toString(x)) << ++counter;
  return newSym(os.str());
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



Cell* type(Cell* x) {
  Cell* result = nil;
  switch(x->type) {
  case NUM:
    result = newSym(L"number"); break;
  case SYM:
    result = newSym(L"symbol"); break;
  case STRING:
    result = newSym(L"string"); break;
  case TABLE:
    result = newSym(L"table"); break;
  case PRIM_FUNC:
    result = newSym(L"function"); break;
  case CONS:
    if (x == nil) break;
    if (car(x) == newSym(L"fn") || car(x) == newSym(L"evald-fn"))
      result = newSym(L"function");
    else if (car(x) == newSym(L"type"))
      result = car(cdr(x));
    else
      result = newSym(L"list");
    break;
  default:
    err << "Undefined type: " << x->type << endl << DIE;
  }
  return result;
}



                                  ostream& operator<<(ostream& os, Cell* c);

                                  ostream& operator<<(ostream& os, Table* t) {
                                    os << "{";
                                    for (CellMap::iterator p = t->table.begin(); p != t->table.end(); ++p) {
                                      if (p->second)
                                        os << (Cell*)p->first << ", ";
                                    }
                                    return os << "}" << endl;
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
                                      os << "(" << (Table*)c->car;
                                      if (cdr(c) != nil)
                                        os << " . " << cdr(c) << ")";
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
