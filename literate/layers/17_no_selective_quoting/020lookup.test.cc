void test_lookup_returns_dynamic_binding() {
  cell* var = new_sym("a");
  cell* val = new_num(34);
  new_dynamic_scope(var, val);
    CHECK_EQ(lookup(var), val);
  end_dynamic_scope(var);
}

void test_new_dynamic_scope_increments_refcounts() {
  cell* var = new_sym("a");
  cell* val = new_num(34);
  CLEAR_TRACE;
  new_dynamic_scope(var, val);
    CHECK_EQ(excess_mkrefs(), 2);   // one for var, one for val
  end_dynamic_scope(var);
}
