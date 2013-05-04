Cell* copyList(Cell* x) {
  if (!isCons(x)) return x;
  return newCons(copyList(car(x)),
      copyList(cdr(x)));
}

bool equalList(Cell* a, Cell* b) {
  if (!isCons(a)) return a == b;
  return equalList(car(a), car(b))
      && equalList(cdr(a), cdr(b));
}

Cell* nthCdr(Cell* x, long n) {
  Cell* curr = x;
  for (long idx = n; idx > 0; --idx) {
    if (!isCons(curr))
      RAISE << "list is too short: " << x << " " << n << endl;
    curr=cdr(curr);
  }
  return curr;
}

void append(Cell* x, Cell* y) {
  while(cdr(x) != nil)
    x = cdr(x);
  setCdr(x, y);
}

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
