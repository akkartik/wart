void test_pointers_from_nil_are_nil() {
  checkEq(nil->car, nil);
  checkEq(nil->cdr, nil);
}

void test_newCell_has_nil_car_and_cdr() {
  Cell* x = newCell();
  checkEq(x->car, nil);
  checkEq(x->cdr, nil);
  rmref(x);
}

void test_rmref_frees_space() {
  Cell* c = newCell();
  checkEq(c->car, nil);
  checkEq(freelist, NULL);
  rmref(c);
  check(!c->car);
  checkEq(freelist, c);
}

void test_rmref_handles_nums() {
  Cell* c = newNum(34);
  rmref(c);
  check(!c->car);
  checkEq(freelist, c);
}

void test_nthCdr() {
  Cell* x = newCons(newNum(3), newCons(newNum(4), nil));
  checkEq(nthCdr(x, 0), x);
  checkEq(car(nthCdr(x, 1)), newNum(4));
  checkEq(nthCdr(x, 2), nil);
  rmref(x);
}

void test_Cell_layout_constraints() {
  // no wasting space
  Cell cell;
  check((sizeof(cell.car)%4) == 0);
  check((sizeof(cell.cdr)%4) == 0);
  check((sizeof(cell.type)%4) == 0);
  check((sizeof(cell.nrefs)%4) == 0);

  // all Cells are the same size so we don't fragment memory
  // (except for strings)

  // car and cdr can store numbers
  check(sizeof(long) <= sizeof(Cell*));
  check(sizeof(float) <= sizeof(Cell*));
  check(sizeof(size_t) <= sizeof(Cell*));
}

void test_contains_handles_circular_lists() {
  unordered_set<Cell*> done;
  Cell* x = newCons(newNum(1), nil);
  setCdr(x, x);
  check(!contains(x, newSym("a"), done));
  x->cdr = nil; // break cycle for gc
  rmref(x);
}
