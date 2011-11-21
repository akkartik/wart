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
