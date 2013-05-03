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
