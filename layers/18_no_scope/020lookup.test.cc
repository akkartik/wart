void test_lookup_returns_dynamic_binding() {
  TEMP(var, new_sym("a"));
  TEMP(val, new_num(34));
  new_binding(var, val);
  CHECK_EQ(lookup(var), val);
}
