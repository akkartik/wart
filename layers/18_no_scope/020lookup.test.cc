void test_lookup_returns_dynamic_binding() {
  Cell* sym = newSym("a");
  checkEq(sym->nrefs, 1);
  Cell* val = newNum(34);
  checkEq(val->nrefs, 1);
  newBinding(sym, val);
    checkEq(lookup(sym), val);
    checkEq(sym->nrefs, 2);
    checkEq(val->nrefs, 2);
}
