void test_lookup_returns_dynamic_binding() {
  cell* sym = new_sym("a");
  CHECK_EQ(sym->nrefs, 1);
  cell* val = new_num(34);
  CHECK_EQ(val->nrefs, 1);
  new_binding(sym, val);
    CHECK_EQ(lookup(sym), val);
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 2);
}
