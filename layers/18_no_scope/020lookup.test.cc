void test_lookup_returns_dynamic_binding() {
  cell* var = new_sym("a");
  cell* val = new_num(34);
  new_binding(var, val);
  CHECK_EQ(lookup(var), val);
}

void test_new_binding_increments_refcounts() {
  cell* var = new_sym("a");
  cell* val = new_num(34);
  CLEAR_TRACE;
  new_binding(var, val);
  CHECK_EQ(excess_mkrefs(), 2);   // one for var, one for val
}
