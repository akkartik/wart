void test_to_float_works() {
  cell* num1 = new_num(3);
  CHECK(equal_floats(to_float(num1), 3.0));
  cell* num2 = new_num(1.5);
  CHECK(equal_floats(to_float(num2), 1.5));
}

void test_integers_are_interned() {
  CLEAR_TRACE;
  cell* num = new_num(23);
  CHECK_EQ(trace_count("gc", "alloc"), 1);
  CHECK_EQ(trace_count("gc", "mkref"), 1);
  CHECK_TRACE_CONTENTS("gc/mkref", "23");
  CHECK_EQ(num->nrefs, 1);
}

void test_floats_are_not_interned() {
  CLEAR_TRACE;
  cell* num = new_num(1.0);
  CHECK_EQ(num->type, FLOAT);
  CHECK_EQ(trace_count("gc", "alloc"), 1);
  CHECK_TRACE_DOESNT_CONTAIN("gc", "mkref");
  CHECK_EQ(num->nrefs, 0);
}

void test_syms_are_interned() {
  CLEAR_TRACE;
  cell* sym = new_sym("a");
  CHECK_EQ(sym->nrefs, 1);
  CHECK_EQ(trace_count("gc", "mkref"), 1);
}

void test_strings_are_not_interned() {
  CLEAR_TRACE;
  cell* s = new_string("a");
  CHECK_EQ(s->nrefs, 0);
  CHECK_TRACE_DOESNT_CONTAIN("gc", "mkref");
}

void test_set_car_increments_nrefs() {
  CLEAR_TRACE;
  TEMP(pair, new_cell());
  cell* a = new_cell();
  set_car(pair, a);
  CHECK_EQ(a->nrefs, 1);
  CHECK_EQ(trace_count("gc", "mkref"), 1);
  // no need to rmref 'a' because it's attached to 'pair'
}

void test_set_car_decrements_nrefs_of_old_car() {
  TEMP(pair, new_cell());
  cell* a = new_cell();
  set_car(pair, a);
  CLEAR_TRACE;
  cell* b = new_cell();
  set_car(pair, b);
  CHECK(is_free(a));
  CHECK_EQ(trace_count("gc", "mkref"), trace_count("gc", "rmref"));
}

void test_set_car_decrements_nrefs_of_interned_cell() {
  TEMP(pair, new_cell());
  cell* a = new_num(4);
  set_car(pair, a);
  CLEAR_TRACE;
  cell* b = new_cell();
  set_car(pair, b);
  CHECK(!is_free(a));
  CHECK_EQ(trace_count("gc", "mkref"), trace_count("gc", "rmref"));
}

void test_set_car_is_idempotent() {
  TEMP(pair, new_cell());
  cell* x = new_cell();
  set_car(pair, x);
  CLEAR_TRACE;
  set_car(pair, x);
  CHECK_EQ(trace_count("gc", "mkref"), trace_count("gc", "rmref"));
}

void test_set_cdr_is_idempotent() {
  TEMP(pair, new_cell());
  cell* x = new_cell();
  set_cdr(pair, x);
  CLEAR_TRACE;
  set_cdr(pair, x);
  CHECK_EQ(trace_count("gc", "mkref"), trace_count("gc", "rmref"));
}

void test_set_increments_nrefs() {
  TEMP(t, new_table());
  cell* key = new_sym("a");
  cell* val = new_num(34);
  CLEAR_TRACE;
  set(t, key, val);
  CHECK_EQ(trace_count("gc", "mkref"), 2);
}

void test_set_decrements_overridden_values() {
  TEMP(t, new_table());
  cell* key = new_sym("a");
  cell* val = new_num(34);
  cell* val2 = new_num(35);
  set(t, key, val);
  CLEAR_TRACE;
  set(t, key, val2);
  CHECK_EQ(trace_count("gc", "rmref"), 1);
  CHECK_TRACE_CONTENTS("gc/rmref", "34");
  CHECK_EQ(trace_count("gc", "mkref"), 1);
  CHECK_TRACE_CONTENTS("gc/mkref", "35");
}

void test_set_decrements_key_on_delete() {
  TEMP(t, new_table());
  cell* key = new_sym("a");
  cell* val = new_num(34);
  set(t, key, val);
  CLEAR_TRACE;
  set(t, key, nil);
  CHECK_EQ(trace_count("gc", "rmref"), 2);
  CHECK_TRACE_CONTENTS("gc/rmref", "a");
  CHECK_TRACE_CONTENTS("gc/rmref", "34");
  CHECK_TRACE_DOESNT_CONTAIN("gc", "mkref");
}

void test_set_deletes_nonexistent_key() {
  TEMP(t, new_table());
  cell* k = new_sym("nonexistent key test");
  CLEAR_TRACE;
  set(t, k, nil);
  CHECK_TRACE_DOESNT_CONTAIN("gc", "mkref");
}
