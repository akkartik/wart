void test_to_float_works() {
  cell* num1 = new_num(3);
  CHECK(equal_floats(to_float(num1), 3.0));
  cell* num2 = new_num(1.5);
  CHECK(equal_floats(to_float(num2), 1.5));
}

void test_set_car_decrements_nrefs() {
  cell* cons = new_cell();
  cell* car = new_cell();
  CHECK_EQ(car->nrefs, 0);
  cell* new_car = new_cell();
  CHECK_EQ(new_car->nrefs, 0);
  set_car(cons, car);
  CHECK_EQ(car->nrefs, 1);
  CHECK_EQ(new_car->nrefs, 0);
  set_car(cons, new_car);
  CHECK_EQ(car->nrefs, 0);
  CHECK_EQ(new_car->nrefs, 1);
  rmref(cons);
}

void test_set_car_decrements_nrefs_for_non_cons() {
  cell* cons = new_cell();
  cell* num = new_num(23);
  CHECK_EQ(num->nrefs, 1);
  cell* new_car = new_cell();
  CHECK_EQ(new_car->nrefs, 0);
  set_car(cons, num);
  CHECK_EQ(num->nrefs, 2);
  CHECK_EQ(new_car->nrefs, 0);
  set_car(cons, new_car);
  CHECK_EQ(num->nrefs, 1);
  CHECK_EQ(new_car->nrefs, 1);
  rmref(cons);
}

void test_set_car_is_idempotent() {
  cell* cons = new_cell();
  cell* x = new_cell();
  CHECK_EQ(x->nrefs, 0);
  set_car(cons, x);
  CHECK_EQ(x->nrefs, 1);
  set_car(cons, x);
  CHECK_EQ(x->nrefs, 1);
  CHECK(car(x));
  CHECK(cdr(x));
  rmref(cons);
}

void test_set_cdr_is_idempotent() {
  cell* cons = new_cell();
  cell* x = new_cell();
  CHECK_EQ(x->nrefs, 0);
  set_cdr(cons, x);
  CHECK_EQ(x->nrefs, 1);
  set_cdr(cons, x);
  CHECK_EQ(x->nrefs, 1);
  CHECK(car(x));
  CHECK(cdr(x));
  rmref(cons);
}

void test_set_deletes_nonexistent_key() {
  cell* t = new_table();
  cell* k = new_sym("nonexistent key test");
  CHECK_EQ(k->nrefs, 1);
  set(t, k, nil);
  CHECK_EQ(k->nrefs, 1);
  rmref(k);
  rmref(t);
}
