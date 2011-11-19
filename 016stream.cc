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
    if (car(c) == newSym("'") || car(c) == newSym("`") || car(c) == newSym(",") || car(c) == newSym(",@") || car(c) == newSym("@"))
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
  return newCons(newSym("type"), newCons(newSym("stream"),
            newCons(newNum((long)x), nil)));
}

Cell* newOstream(ostream* x) {
  return newCons(newSym("type"), newCons(newSym("stream"),
            newCons(newNum((long)x), nil)));
}

void setupStreams() {
  newDynamicScope(newSym("stdin"), newIstream(&cin));
  newDynamicScope(newSym("stdout"), newOstream(&cout));
  newDynamicScope(newSym("stderr"), newOstream(&cerr));
}
#define STDIN dynamics[newSym("stdin")].top()
#define STDOUT dynamics[newSym("stdout")].top()
#define STDERR dynamics[newSym("stderr")].top()
void teardownStreams() {
  endDynamicScope(newSym("stdin"));
  endDynamicScope(newSym("stdout"));
  endDynamicScope(newSym("stderr"));
}

istream& toIstream(Cell* x) {
  return *(istream*)toNum(car(cdr(cdr(x))));
}

ostream& toOstream(Cell* x) {
  return *(ostream*)toNum(car(cdr(cdr(x))));
}

COMPILE_PRIM_FUNC(pr, primFunc_pr, "($x)",
  Cell* x = lookup("$x");
  ostream& out = toOstream(STDOUT);
  print(x, out);
  out.flush();
  return mkref(x);
)

COMPILE_PRIM_FUNC(write, primFunc_write, "($x)",
  Cell* x = lookup("$x");
  ostream& out = toOstream(STDOUT);
  write(x, out);
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



COMPILE_PRIM_FUNC(infile, primFunc_infile, "($name)",
  return mkref(newIstream(new ifstream(toString(lookup("$name")).c_str(), std::ios::binary)));
)

COMPILE_PRIM_FUNC(close_infile, primFunc_close_infile, "($stream)",
  ifstream* f = (ifstream*)toNum(car(cdr(cdr(lookup("$stream")))));
  f->close();
  delete f;
  return nil;
)

COMPILE_PRIM_FUNC(outfile, primFunc_outfile, "($name)",
  return mkref(newOstream(new ofstream(toString(lookup("$name")).c_str(), std::ios::binary)));
)

COMPILE_PRIM_FUNC(close_outfile, primFunc_close_outfile, "($stream)",
  ofstream* f = (ofstream*)toNum(car(cdr(cdr(lookup("$stream")))));
  f->close();
  delete f;
  return nil;
)

COMPILE_PRIM_FUNC(err, primFunc_err, "($x)",
  Cell* x = lookup("$x");
  ostream& out = toOstream(STDERR);
  print(x, out);
  out.flush();
  return mkref(x);
)

COMPILE_PRIM_FUNC(instring, primFunc_instring, "($s)",
  return mkref(newIstream(new stringstream(toString(lookup("$s")))));
)

COMPILE_PRIM_FUNC(outstring, primFunc_outstring, "()",
  return mkref(newOstream(new ostringstream()));
)

COMPILE_PRIM_FUNC(outstring_buffer, primFunc_outstring_buffer, "($stream)",
  ostringstream* s = (ostringstream*)toNum(car(cdr(cdr(lookup("$stream")))));
  return mkref(newString(s->str()));
)
