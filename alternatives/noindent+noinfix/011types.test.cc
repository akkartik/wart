void test_toFloat_works() {
  Cell* num1 = newNum(3);
  check(equalFloats(toFloat(num1), 3.0));
  Cell* num2 = newNum(1.5);
  check(equalFloats(toFloat(num2), 1.5));
  rmref(num2);
  rmref(num1);
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

void test_set_deletes_nonexistent_key() {
  Cell* t = newTable();
  Cell* k = newCons(newNum(375));
  set(t, k, nil);
  rmref(k);
  rmref(t);
}
