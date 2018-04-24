void test_to_float_works() {
  TEMP(num1, mkref(new_num(3)));
  CHECK(equal_floats(to_float(num1), 3.0));
  TEMP(num2, mkref(new_num(1.5)));
  CHECK(equal_floats(to_float(num2), 1.5));
}

void test_integers_are_interned() {
  TEMP(num, mkref(new_num(23)));
  CHECK_EQ(num->nrefs, 2);  // one extra for intern table
}

void test_floats_are_not_interned() {
  TEMP(num, mkref(new_num(1.0)));
  CHECK_EQ(num->nrefs, 1);
}

void test_syms_are_interned() {
  TEMP(sym, mkref(new_sym("a")));
  CHECK_EQ(sym->nrefs, 2);  // one extra for intern table
}

void test_strings_are_not_interned() {
  TEMP(s, mkref(new_string("a")));
  CHECK_EQ(s->nrefs, 1);
}

void test_set_car_increments_nrefs() {
  TEMP(pair, mkref(new_cell()));
  TEMP(a, mkref(new_cell()));
  CHECK_EQ(a->nrefs, 1);
  set_car(pair, a);
  CHECK_EQ(a->nrefs, 2);
}

void test_set_car_decrements_nrefs_of_old_car() {
  TEMP(pair, mkref(new_cell()));
  TEMP(a, mkref(new_cell()));
  CHECK_EQ(a->nrefs, 1);
  set_car(pair, a);
  CHECK_EQ(a->nrefs, 2);
  TEMP(b, mkref(new_cell()));
  set_car(pair, b);
  CHECK_EQ(a->nrefs, 1);
}

void test_set_car_is_idempotent() {
  TEMP(pair, mkref(new_cell()));
  TEMP(a, mkref(new_cell()));
  CHECK_EQ(a->nrefs, 1);
  set_car(pair, a);
  CHECK_EQ(a->nrefs, 2);
  set_car(pair, a);
  CHECK_EQ(a->nrefs, 2);
}

void test_set_cdr_is_idempotent() {
  TEMP(pair, mkref(new_cell()));
  TEMP(a, mkref(new_cell()));
  CHECK_EQ(a->nrefs, 1);
  set_cdr(pair, a);
  CHECK_EQ(a->nrefs, 2);
  set_cdr(pair, a);
  CHECK_EQ(a->nrefs, 2);
}

void test_set_cdr_on_atoms_warns() {
  Hide_warnings = true;
  TEMP(x, mkref(new_cell()));
  TEMP(integer, mkref(new_num(3)));
  set_cdr(integer, x);
  CHECK_TRACE_WARNS();

  TEMP(real, mkref(new_num(3.14)));
  CLEAR_TRACE;
  set_cdr(real, x);
  CHECK_TRACE_WARNS();

  TEMP(sym, mkref(new_sym("a")));
  CLEAR_TRACE;
  set_cdr(sym, x);
  CHECK_TRACE_WARNS();

  TEMP(str, mkref(new_string("a")));
  CLEAR_TRACE;
  set_cdr(str, x);
  CHECK_TRACE_WARNS();
}

void test_set_increments_nrefs() {
  TEMP(t, mkref(new_table()));
  TEMP(key, mkref(new_sym("a")));
  TEMP(val, mkref(new_num(34)));
  int nk=key->nrefs,  nv=val->nrefs;
  put(t, key, val);
  CHECK_EQ(nk+1, key->nrefs);
  CHECK_EQ(nv+1, val->nrefs);
}

void test_set_decrements_overridden_values() {
  TEMP(t, mkref(new_table()));
  TEMP(key, mkref(new_sym("a")));
  TEMP(val, mkref(new_num(34)));
  TEMP(val2, mkref(new_num(35)));
  put(t, key, val);
  int nv=val->nrefs;
  put(t, key, val2);
  CHECK_EQ(nv-1, val->nrefs);
}

void test_set_decrements_key_on_delete() {
  TEMP(t, mkref(new_table()));
  TEMP(key, mkref(new_sym("a")));
  TEMP(val, mkref(new_num(34)));
  put(t, key, val);
  int nv=val->nrefs;
  put(t, key, nil);
  CHECK_EQ(nv-1, val->nrefs);
}

void test_set_ignores_nonexistent_key() {
  TEMP(t, mkref(new_table()));
  TEMP(k, mkref(new_sym("nonexistent key test")));
  int nk = k->nrefs;
  put(t, k, nil);
  CHECK_EQ(nk, k->nrefs);
}
