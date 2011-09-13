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
  Cell* c = newCell();
  c->type = NUM;
  c->car = (Cell*)34;
  rmref(c);
  check(!c->car);
  checkEq(freelist, c);
}



void test_setCar_decrements_nrefs() {
  Cell* cons = newCell();
  Cell* car = newCell();
  checkEq(car->nrefs, 0);
  Cell* newCar = newCell();
  checkEq(newCar->nrefs, 0);
  setCar(cons, car);
  checkEq(car->nrefs, 1);
  checkEq(newCar->nrefs, 0);
  setCar(cons, newCar);
  checkEq(car->nrefs, 0);
  checkEq(newCar->nrefs, 1);
  rmref(cons);
}

void test_setCar_decrements_nrefs_for_non_cons() {
  Cell* cons = newCell();
  Cell* num = newNum(23);
  checkEq(num->nrefs, 1);
  Cell* newCar = newCell();
  checkEq(newCar->nrefs, 0);
  setCar(cons, num);
  checkEq(num->nrefs, 2);
  checkEq(newCar->nrefs, 0);
  setCar(cons, newCar);
  checkEq(num->nrefs, 1);
  checkEq(newCar->nrefs, 1);
  rmref(cons);
}

void test_setCar_is_idempotent() {
  Cell* cons = newCell();
  Cell* x = newCell();
  checkEq(x->nrefs, 0);
  setCar(cons, x);
  checkEq(x->nrefs, 1);
  setCar(cons, x);
  checkEq(x->nrefs, 1);
  check(car(x));
  check(cdr(x));
  rmref(cons);
}

void test_setCdr_is_idempotent() {
  Cell* cons = newCell();
  Cell* x = newCell();
  checkEq(x->nrefs, 0);
  setCdr(cons, x);
  checkEq(x->nrefs, 1);
  setCdr(cons, x);
  checkEq(x->nrefs, 1);
  check(car(x));
  check(cdr(x));
  rmref(cons);
}
