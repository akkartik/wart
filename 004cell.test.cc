void test_pointers_from_nil_are_nil() {
  check_eq(nil->car, nil);
  check_eq(nil->cdr, nil);
}

void test_newCell_has_nil_car_and_cdr() {
  Cell* x = newCell();
  check_eq(x->car, nil);
  check_eq(x->cdr, nil);
  rmref(x);
}

void test_rmref_frees_space() {
  Cell* c = newCell();
  check_eq(c->car, nil);
  check_eq(freelist, NULL);
  rmref(c);
  check(!c->car);
  check_eq(freelist, c);
}

void test_rmref_handles_nums() {
  Cell* c = newCell();
  c->type = NUM;
  c->car = (Cell*)34;
  rmref(c);
  check(!c->car);
  check_eq(freelist, c);
}



void test_setCar_decrements_nrefs() {
  Cell* cons = newCell();
  Cell* car = newCell();
  check_eq(car->nrefs, 0);
  Cell* newCar = newCell();
  check_eq(newCar->nrefs, 0);
  setCar(cons, car);
  check_eq(car->nrefs, 1);
  check_eq(newCar->nrefs, 0);
  setCar(cons, newCar);
  check_eq(car->nrefs, 0);
  check_eq(newCar->nrefs, 1);
  rmref(cons);
}

void test_setCar_decrements_nrefs_for_non_cons() {
  Cell* cons = newCell();
  Cell* num = newNum(23);
  check_eq(num->nrefs, 1);
  Cell* newCar = newCell();
  check_eq(newCar->nrefs, 0);
  setCar(cons, num);
  check_eq(num->nrefs, 2);
  check_eq(newCar->nrefs, 0);
  setCar(cons, newCar);
  check_eq(num->nrefs, 1);
  check_eq(newCar->nrefs, 1);
  rmref(cons);
}

void test_setCar_is_idempotent() {
  Cell* cons = newCell();
  Cell* x = newCell();
  check_eq(x->nrefs, 0);
  setCar(cons, x);
  check_eq(x->nrefs, 1);
  setCar(cons, x);
  check_eq(x->nrefs, 1);
  check(car(x));
  check(cdr(x));
  rmref(cons);
}

void test_setCdr_is_idempotent() {
  Cell* cons = newCell();
  Cell* x = newCell();
  check_eq(x->nrefs, 0);
  setCdr(cons, x);
  check_eq(x->nrefs, 1);
  setCdr(cons, x);
  check_eq(x->nrefs, 1);
  check(car(x));
  check(cdr(x));
  rmref(cons);
}
