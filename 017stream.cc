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
    if (car(c) == newSym(L"'") || car(c) == newSym(L"`") || car(c) == newSym(L",") || car(c) == newSym(L",@") || car(c) == newSym(L"@"))
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

void write(Cell* x, ostream& out) {
  out << x;
  printDepth=0;
}

void print(Cell* x, ostream& out) {
  if (isString(x)) out << toString(x);
  else out << x;
  printDepth=0;
}



Cell* newIstream(istream* x) {
  return newCons(newSym(L"type"), newCons(newSym(L"stream"),
            newCons(newNum((long)x), nil)));
}

Cell* newOstream(ostream* x) {
  return newCons(newSym(L"type"), newCons(newSym(L"stream"),
            newCons(newNum((long)x), nil)));
}

void setupStreams() {
  newDynamicScope(newSym(L"stdin"), newIstream(&cin));
  newDynamicScope(newSym(L"stdout"), newOstream(&cout));
  newDynamicScope(newSym(L"stderr"), newOstream(&cerr));
}
#define STDIN dynamics[newSym(L"stdin")].top()
#define STDOUT dynamics[newSym(L"stdout")].top()
#define STDERR dynamics[newSym(L"stderr")].top()
void teardownStreams() {
  endDynamicScope(newSym(L"stdin"));
  endDynamicScope(newSym(L"stdout"));
  endDynamicScope(newSym(L"stderr"));
}

istream& toIstream(Cell* x) {
  return *(istream*)toNum(car(cdr(cdr(x))));
}

ostream& toOstream(Cell* x) {
  return *(ostream*)toNum(car(cdr(cdr(x))));
}

COMPILE_PRIM_FUNC(pr, primFunc_pr, L"($x)",
  Cell* x = lookup(L"$x");
  ostream& out = toOstream(STDOUT);
  print(x, out);
  out.flush();
  return mkref(x);
)

COMPILE_PRIM_FUNC(write, primFunc_write, L"($x)",
  Cell* x = lookup(L"$x");
  ostream& out = toOstream(STDOUT);
  write(x, out);
  out.flush();
  return mkref(x);
)

COMPILE_PRIM_FUNC(read, primFunc_read, L"('$eof)",
  if (toIstream(STDIN).eof()) return lookup(L"$eof");
  return mkref(wartRead(toIstream(STDIN)).front());
)

COMPILE_PRIM_FUNC(infile, primFunc_infile, L"($name)",
  return mkref(newIstream(new ifstream(&toAscii(toString(lookup(L"$name")))[0])));
)

COMPILE_PRIM_FUNC(close_infile, primFunc_close_infile, L"($stream)",
  ifstream* f = (ifstream*)toNum(car(cdr(cdr(lookup(L"$stream")))));
  f->close();
  delete f;
  return nil;
)

COMPILE_PRIM_FUNC(outfile, primFunc_outfile, L"($name)",
  return mkref(newOstream(new ofstream(&toAscii(toString(lookup(L"$name")))[0])));
)

COMPILE_PRIM_FUNC(close_outfile, primFunc_close_outfile, L"($stream)",
  ofstream* f = (ofstream*)toNum(car(cdr(cdr(lookup(L"$stream")))));
  f->close();
  delete f;
  return nil;
)

COMPILE_PRIM_FUNC(err, primFunc_err, L"($x)",
  Cell* x = lookup(L"$x");
  ostream& out = toOstream(STDERR);
  print(x, out);
  out.flush();
  return mkref(x);
)

COMPILE_PRIM_FUNC(sym, primFunc_sym, L"$args",
  ostringstream out;
  for (Cell* args = lookup(L"$args"); args != nil; args = cdr(args))
    print(car(args), out);
  return mkref(newSym(out.str()));
)
