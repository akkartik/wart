                                  ostream& operator<<(ostream& os, Table* t) {
                                    os << "{";
                                    for (CellMap::iterator p = t->table.begin(); p != t->table.end(); ++p) {
                                      if (p->second)
                                        os << (Cell*)p->first << ", ";
                                    }
                                    return os << "}";
                                  }

ostream& operator<<(ostream& os, Cell* c) {
  if (c == NULL) return os << "NULLNULLNULL";
  if (c == nil) return os << "nil";
  switch(c->type) {
  case CONS:
    if (car(c) == newSym("'") || car(c) == newSym("`") || car(c) == newSym(",") || car(c) == newSym(",@") || car(c) == newSym("@"))
      return os << car(c) << cdr(c);
    os << "(" << car(c);
    for (Cell* curr = cdr(c); curr != nil; curr = cdr(curr)) {
      if (isCons(curr))
        os << " " << car(curr);
      else
        os << " . " << curr;
    }
    return os << ")";
  case NUMBER:
    return os << toNum(c);
  case SYMBOL:
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

void print(Cell* x, ostream& out) {
  if (isString(x)) out << toString(x);
  else out << x;
}



COMPILE_PRIM_FUNC(pr, primFunc_pr, "($x)",
  Cell* x = lookup("$x");
  ostream& out = toOstream(STDOUT);
  print(x, out);
  out.flush();
  return mkref(x);
)

COMPILE_PRIM_FUNC(dbg, primFunc_dbg, "($x)",
  dbg << lookup("$x") << endl;
  return nil;
)

COMPILE_PRIM_FUNC(write, primFunc_write, "($x)",
  Cell* x = lookup("$x");
  ostream& out = toOstream(STDOUT);
  out << x;
  out.flush();
  return mkref(x);
)

COMPILE_PRIM_FUNC(err, primFunc_err, "($x)",
  Cell* x = lookup("$x");
  ostream& out = toOstream(STDERR);
  print(x, out);
  out.flush();
  return mkref(x);
)

                                  Cell* read(istream& in) {
                                    CodeStream c(in);
                                    return read(c);
                                  }

COMPILE_PRIM_FUNC(read, primFunc_read, "('$eof)",
  if (toIstream(STDIN).eof())
    return mkref(lookup("$eof"));
  return mkref(read(toIstream(STDIN)));
)

COMPILE_PRIM_FUNC(read-byte, primFunc_read_byte, "('$eof)",
  istream& f = toIstream(STDIN);
  if (f.eof())
    return mkref(lookup("$eof"));
  char c;
  f.read(&c, 1);
  return mkref(newNum((long)c));
)
