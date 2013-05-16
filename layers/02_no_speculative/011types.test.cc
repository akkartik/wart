void test_toFloat_works() {
  Cell* num1 = newNum(3);
  CHECK(equalFloats(toFloat(num1), 3.0));
  Cell* num2 = newNum(1.5);
  CHECK(equalFloats(toFloat(num2), 1.5));
  rmref(num2);
  rmref(num1);
}

void test_setCar_decrements_nrefs() {
  Cell* cons = newCell();
  Cell* car = newCell();
  CHECK_EQ(car->nrefs, 0);
  Cell* newCar = newCell();
  CHECK_EQ(newCar->nrefs, 0);
  setCar(cons, car);
  CHECK_EQ(car->nrefs, 1);
  CHECK_EQ(newCar->nrefs, 0);
  setCar(cons, newCar);
  CHECK_EQ(car->nrefs, 0);
  CHECK_EQ(newCar->nrefs, 1);
  rmref(cons);
}

void test_setCar_decrements_nrefs_for_non_cons() {
  Cell* cons = newCell();
  Cell* num = newNum(23);
  CHECK_EQ(num->nrefs, 1);
  Cell* newCar = newCell();
  CHECK_EQ(newCar->nrefs, 0);
  setCar(cons, num);
  CHECK_EQ(num->nrefs, 2);
  CHECK_EQ(newCar->nrefs, 0);
  setCar(cons, newCar);
  CHECK_EQ(num->nrefs, 1);
  CHECK_EQ(newCar->nrefs, 1);
  rmref(cons);
}

void test_setCar_is_idempotent() {
  Cell* cons = newCell();
  Cell* x = newCell();
  CHECK_EQ(x->nrefs, 0);
  setCar(cons, x);
  CHECK_EQ(x->nrefs, 1);
  setCar(cons, x);
  CHECK_EQ(x->nrefs, 1);
  CHECK(car(x));
  CHECK(cdr(x));
  rmref(cons);
}

void test_setCdr_is_idempotent() {
  Cell* cons = newCell();
  Cell* x = newCell();
  CHECK_EQ(x->nrefs, 0);
  setCdr(cons, x);
  CHECK_EQ(x->nrefs, 1);
  setCdr(cons, x);
  CHECK_EQ(x->nrefs, 1);
  CHECK(car(x));
  CHECK(cdr(x));
  rmref(cons);
}

void test_set_deletes_nonexistent_key() {
  Cell* t = newTable();
  Cell* k = newSym("nonexistent key test");
  CHECK_EQ(k->nrefs, 1);
  set(t, k, nil);
  CHECK_EQ(k->nrefs, 1);
  rmref(k);
  rmref(t);
}
