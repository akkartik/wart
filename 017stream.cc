                                  ostream& operator<<(ostream& os, Table* t) {
                                    os << "{";
                                    for (CellMap::iterator p = t->table.begin(); p != t->table.end(); ++p) {
                                      if (p->second)
                                        os << (Cell*)p->first << ", ";
                                    }
                                    return os << "}";
                                  }

int printDepth = 0;
ostream& operator<<(ostream& os, Cell* c) {
  if (c == NULL) return os << "NULLNULLNULL";
  if (c == nil) return os << "nil";
  if (++printDepth > 512) return os << "...";
  switch(c->type) {
  case CONS:
    if (car(c) == newSym(L"'") || car(c) == newSym(L"`") || car(c) == newSym(L",") || car(c) == newSym(L",@"))
      return os << car(c) << cdr(c);
    os << "(" << car(c);
    for (Cell* curr = cdr(c); curr != nil; curr = cdr(curr)) {
      if (++printDepth > 512)
        return os << "...";
      if (isCons(curr))
        os << " " << car(curr);
      else
        os << " . " << curr;
    }
    return os << ")";
  case NUM:
    return os << toNum(c);
  case SYM:
    return os << toString(c);
  case STRING:
    return os << "\"" << toString(c) << "\"";
  case TABLE:
    os << (Table*)c->car;
    if (cdr(c) != nil)
      os << "->" << cdr(c);
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



COMPILE_PRIM_FUNC(sym, primFunc_sym, L"$args",
  ostringstream out;
  for (Cell* args = lookup(L"$args"); args != nil; args = cdr(args))
    if (isString(car(args))) out << toString(car(args));
    else out << car(args);
  printDepth=0;
  return mkref(newSym(out.str()));
)

COMPILE_PRIM_FUNC(pr, primFunc_prn, L"($x)",
  Cell* x = lookup(L"$x");
  if (isString(x)) cout << toString(x);
  else cout << x;
  printDepth=0;
  cout.flush();
  return mkref(x);
)
