void test_lookup_returns_dynamic_binding() {
  cell* sym = new_sym("a");
  cell* val = new_num(34);
  newBinding(sym, val);
  CHECK_EQ(lookup(sym), val);
}
