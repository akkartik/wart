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

void addCons(Cell* p, Cell* x) {
  setCdr(p, newCons(x));
}
