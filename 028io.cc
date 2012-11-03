//// compiled primitives for I/O

ostream& operator<<(ostream& os, Cell* c) {
  if (c == NULL) return os << "NULLNULLNULL";
  if (c == nil) return os << "nil";
  switch(c->type) {
  case CONS:
    if (car(c) == sym_quote || car(c) == sym_backquote || car(c) == sym_unquote || car(c) == sym_splice || car(c) == sym_unquoteSplice)
      return os << car(c) << cdr(c);
    os << "(" << car(c);
    for (Cell* curr = cdr(c); curr != nil; curr = cdr(curr)) {
      if (isCons(curr))
        os << " " << car(curr);
      else
        os << " ... " << curr;
    }
    return os << ")";
  case INTEGER:
    return os << toInt(c);
  case FLOAT:
    return os << toFloat(c);
  case SYMBOL:
    return os << toString(c);
  case STRING:
    return os << "\"" << toString(c) << "\"";
  case TABLE:
    os << (Table*)c->car;
    if (cdr(c) != nil)
      os << "->" << cdr(c);
    return os;
  case COMPILED_FN:
    return os << "#compiled";
  default:
    return os << "Can't print type " << c->type << endl << DIE;
  }
}

ostream& operator<<(ostream& os, Table* t) {
  os << "{";
  if (t->table[sym_name]) os << t->table[sym_name] << ": ";
  for (CellMap::iterator p = t->table.begin(); p != t->table.end(); ++p) {
    if (p->second && p->first != sym_name)
      os << (Cell*)p->first << ", ";
  }
  return os << "}";
}



COMPILE_FN(pr, compiledFn_pr, "($x)",
  Cell* x = lookup("$x");
  ostream& out = toOstream(STDOUT);
  print(x, out);
  out.flush();
  return mkref(x);
)

COMPILE_FN(dbg, compiledFn_dbg, "($x)",
  dbg << lookup("$x") << endl;
  return nil;
)

COMPILE_FN(write, compiledFn_write, "($x)",
  Cell* x = lookup("$x");
  ostream& out = toOstream(STDOUT);
  out << x;
  out.flush();
  return mkref(x);
)

COMPILE_FN(err, compiledFn_err, "($x)",
  Cell* x = lookup("$x");
  ostream& out = toOstream(STDERR);
  print(x, out);
  out.flush();
  return mkref(x);
)

void print(Cell* x, ostream& out) {
  if (isString(x)) out << toString(x);
  else out << x;
}



COMPILE_FN(read, compiledFn_read, "('$eof)",
  if (toIstream(STDIN).eof())
    return mkref(lookup("$eof"));
  return mkref(read(toIstream(STDIN)));
)

Cell* read(istream& in) {
  CodeStream cs(in);
  return read(cs);
}

COMPILE_FN(read_byte, compiledFn_read_byte, "('$eof)",
  istream& f = toIstream(STDIN);
  if (f.eof())
    return mkref(lookup("$eof"));
  char c;
  f.read(&c, 1);
  return mkref(newNum((long)c));
)
