void test_lookup_returns_dynamic_binding() {
  Cell* sym = newSym("a");
  CHECK_EQ(sym->nrefs, 1);
  Cell* val = newNum(34);
  CHECK_EQ(val->nrefs, 1);
  newBinding(sym, val);
    CHECK_EQ(lookup(sym), val);
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 2);
}
