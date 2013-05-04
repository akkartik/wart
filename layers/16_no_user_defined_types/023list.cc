// useful idiom: create a dummy cell p, keep appending to it using addCons,
// then return dropPtr(p) which GC's the dummy but mkrefs the rest.
Cell* dropPtr(Cell* p) {
  Cell* x = mkref(cdr(p));
  rmref(p);
  return x;
}

void addCons(Cell* p, Cell* x) {
  setCdr(p, newCons(x));
}

// push p onto l and move one refcount to new head
Cell* pushCons(Cell* p, Cell* l) {
  Cell* result = newCons(p, l);
  rmref(cdr(result));
  return mkref(result);
}
