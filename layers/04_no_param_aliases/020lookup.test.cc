void test_lexical_scope_has_nil_cdr_on_startup() {
  CHECK_EQ(currLexicalScopes.size(), 1);
  CHECK_EQ(cdr(currLexicalScope), nil);
}

void test_lookup_returns_dynamic_binding() {
  Cell* sym = newSym("a");
  CHECK_EQ(sym->nrefs, 1);
  Cell* val = newNum(34);
  CHECK_EQ(val->nrefs, 1);
  newDynamicScope(sym, val);
    CHECK_EQ(lookup(sym), val);
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 2);
  endDynamicScope(sym);
  CHECK_EQ(sym->nrefs, 1);
  CHECK_EQ(val->nrefs, 1);
}

void test_lookup_returns_lexical_binding() {
  Cell* sym = newSym("a");
  CHECK_EQ(sym->nrefs, 1);
  Cell* val = newNum(34);
  CHECK_EQ(val->nrefs, 1);
  newLexicalScope();
    addLexicalBinding(sym, val);
      CHECK_EQ(lookup(sym), val);
      CHECK_EQ(sym->nrefs, 2);
      CHECK_EQ(val->nrefs, 2);
  endLexicalScope();
  CHECK_EQ(sym->nrefs, 1);
  CHECK_EQ(val->nrefs, 1);
}

void test_lexical_binding_always_overrides_dynamic() {
  Cell* sym = newSym("a");
  CHECK_EQ(sym->nrefs, 1);
  Cell* val = newNum(34);
  CHECK_EQ(val->nrefs, 1);
  Cell* dynVal = newNum(35);
  CHECK_EQ(dynVal->nrefs, 1);
  newDynamicScope(sym, dynVal);
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 1);
    CHECK_EQ(dynVal->nrefs, 2);
    newLexicalScope();
      addLexicalBinding(sym, val);
        CHECK_EQ(lookup(sym), val);
        CHECK_EQ(sym->nrefs, 3);
        CHECK_EQ(val->nrefs, 2);
        CHECK_EQ(dynVal->nrefs, 2);
    endLexicalScope();

    CHECK_EQ(lookup(sym), newNum(35));
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 1);
    CHECK_EQ(dynVal->nrefs, 2);
  endDynamicScope(sym);
  CHECK_EQ(sym->nrefs, 1);
  CHECK_EQ(val->nrefs, 1);
  CHECK_EQ(dynVal->nrefs, 1);
}

void test_nil_lexical_binding_works() {
  Cell* sym = newSym("a");
  CHECK_EQ(sym->nrefs, 1);
  Cell* dynVal = newNum(35);
  newDynamicScope(sym, dynVal);
    newLexicalScope();
      addLexicalBinding(sym, nil);
        CHECK_EQ(lookup(sym), nil);
    endLexicalScope();
  endDynamicScope(sym);
}

void test_lexical_scopes_nest_correctly() {
  Cell* sym = newSym("a");
  CHECK_EQ(sym->nrefs, 1);
  Cell* val = newNum(34);
  CHECK_EQ(val->nrefs, 1);
  Cell* val2 = newNum(35);
  CHECK_EQ(val->nrefs, 1);
  Cell* dynVal = newNum(36);
  CHECK_EQ(dynVal->nrefs, 1);
  newDynamicScope(sym, dynVal);
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 1);
    CHECK_EQ(val2->nrefs, 1);
    CHECK_EQ(dynVal->nrefs, 2);
    newLexicalScope();
      CHECK(currLexicalScope != nil);
      CHECK_EQ(currLexicalScope->nrefs, 1);
      addLexicalBinding(sym, val);
        CHECK_EQ(lookup(sym), val);
        CHECK_EQ(sym->nrefs, 3);
        CHECK_EQ(val->nrefs, 2);
        CHECK_EQ(val2->nrefs, 1);
        CHECK_EQ(dynVal->nrefs, 2);
        newLexicalScope();
          CHECK_EQ(cdr(currLexicalScope)->nrefs, 2);
          CHECK_EQ(currLexicalScope->nrefs, 1);
          addLexicalBinding(sym, val2);
            CHECK_EQ(lookup(sym), val2);
            CHECK_EQ(sym->nrefs, 4);
            CHECK_EQ(val->nrefs, 2);
            CHECK_EQ(val2->nrefs, 2);
            CHECK_EQ(dynVal->nrefs, 2);
        endLexicalScope();
      CHECK_EQ(currLexicalScope->nrefs, 1);
      CHECK_EQ(sym->nrefs, 3);
      CHECK_EQ(val->nrefs, 2);
      CHECK_EQ(val2->nrefs, 1);
      CHECK_EQ(dynVal->nrefs, 2);
    endLexicalScope();
    CHECK_EQ(lookup(sym), dynVal);
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 1);
    CHECK_EQ(val2->nrefs, 1);
    CHECK_EQ(dynVal->nrefs, 2);
  endDynamicScope(sym);
  CHECK_EQ(sym->nrefs, 1);
  CHECK_EQ(val->nrefs, 1);
  CHECK_EQ(val2->nrefs, 1);
  CHECK_EQ(dynVal->nrefs, 1);
}

void test_lower_lexical_scopes_are_available() {
  Cell* sym = newSym("a");
  CHECK_EQ(sym->nrefs, 1);
  Cell* val = newNum(34);
  CHECK_EQ(val->nrefs, 1);
  newLexicalScope();
    addLexicalBinding(sym, val);
      CHECK_EQ(lookup(sym), val);
      newLexicalScope();
        CHECK_EQ(lookup(sym), val);
      endLexicalScope();
  endLexicalScope();
}
