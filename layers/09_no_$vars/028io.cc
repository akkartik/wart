//// compiled primitives for I/O

ostream& operator<<(ostream& os, cell* c) {
  if (c == NULL) return os << "NULLNULLNULL";
  if (c == nil) return os << "nil";
  switch(c->type) {
  case CONS:
    if (car(c) == sym_quote || car(c) == sym_backquote || car(c) == sym_unquote || car(c) == sym_splice || car(c) == sym_unquote_splice)
      return os << car(c) << cdr(c);
    os << "(" << car(c);
    for (cell* curr = cdr(c); curr != nil; curr = cdr(curr)) {
      if (is_cons(curr))
        os << " " << car(curr);
      else
        os << " ... " << curr;
    }
    return os << ")";
  case INTEGER:
    return os << to_int(c);
  case FLOAT:
    return os << to_float(c);
  case SYMBOL:
    return os << to_string(c);
  case STRING:
    return os << "\"" << to_string(c) << "\"";
  case TABLE:
    os << (table*)c->car;
    if (cdr(c) != nil)
      os << "->" << cdr(c);
    return os;
  case COMPILED_FN:
    return os << "#compiled";
  default:
    return os << "Can't print type " << c->type << endl << die();
  }
}

ostream& operator<<(ostream& os, table* t) {
  os << "{";
  if (t->value[sym_name]) os << t->value[sym_name] << ": ";
  for (cell_map::iterator p = t->value.begin(); p != t->value.end(); ++p) {
    if (p->second && p->first != sym_name)
      os << (cell*)p->first << ", ";
  }
  return os << "}";
}



COMPILE_FN(pr, compiledfn_pr, "($x)",
  cell* x = lookup("$x");
  ostream& out = to_ostream(STDOUT);
  print(x, out);
  out.flush();
  return mkref(x);
)

COMPILE_FN(write, compiledfn_write, "($x)",
  cell* x = lookup("$x");
  ostream& out = to_ostream(STDOUT);
  out << x;
  out.flush();
  return mkref(x);
)

COMPILE_FN(err, compiledfn_err, "($x)",
  cell* x = lookup("$x");
  ostream& out = to_ostream(STDERR);
  print(x, out);
  out.flush();
  return mkref(x);
)

void print(cell* x, ostream& out) {
  if (is_string(x)) out << to_string(x);
  else out << x;
}



COMPILE_FN(read, compiledfn_read, "('$eof)",
  if (to_istream(STDIN).eof())
    return mkref(lookup("$eof"));
  return mkref(read(to_istream(STDIN)));
)

COMPILE_FN(read_byte, compiledfn_read_byte, "('$eof)",
  istream& f = to_istream(STDIN);
  if (f.eof())
    return mkref(lookup("$eof"));
  char c;
  f.read(&c, 1);
  return mkref(new_num((long)c));
)

COMPILE_FN(read_line, compiledfn_read_line, "('$eof)",
  istream& f = to_istream(STDIN);
  if (f.eof())
    return mkref(lookup("$eof"));
  string result;
  getline(f, result);
  return mkref(new_string(result));
)
