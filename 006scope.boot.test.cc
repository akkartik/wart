void test_lexical_scope_has_nil_cdr_on_startup() {
  check_eq(currLexicalScopes.size(), 1);
  Cell* currLexicalScope = currLexicalScopes.top();
  check_eq(cdr(currLexicalScope), nil);
}

void test_lookup_returns_dynamic_binding() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  newDynamicScope(sym, val);
    check_eq(lookup(sym), val);
    check_eq(sym->nrefs, 2);
    check_eq(val->nrefs, 2);
  endDynamicScope(sym);
  check_eq(sym->nrefs, 1);
  check_eq(val->nrefs, 1);
  checkState();
}

void test_lookup_returns_lexical_binding() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  newLexicalScope();
    addLexicalBinding(sym, val);
      check_eq(lookup(sym), val);
      check_eq(sym->nrefs, 2);
      check_eq(val->nrefs, 2);
  endLexicalScope();
  check_eq(sym->nrefs, 1);
  check_eq(val->nrefs, 1);
  checkState();
}

void test_lexical_binding_always_overrides_dynamic() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  Cell* dynVal = newNum(35);
  check_eq(dynVal->nrefs, 1);
  newDynamicScope(sym, dynVal);
    check_eq(sym->nrefs, 2);
    check_eq(val->nrefs, 1);
    check_eq(dynVal->nrefs, 2);
    newLexicalScope();
      addLexicalBinding(sym, val);
        check_eq(lookup(sym), val);
        check_eq(sym->nrefs, 3);
        check_eq(val->nrefs, 2);
        check_eq(dynVal->nrefs, 2);
    endLexicalScope();

    check_eq(lookup(sym), newNum(35));
    check_eq(sym->nrefs, 2);
    check_eq(val->nrefs, 1);
    check_eq(dynVal->nrefs, 2);
  endDynamicScope(sym);
  check_eq(sym->nrefs, 1);
  check_eq(val->nrefs, 1);
  check_eq(dynVal->nrefs, 1);
  checkState();
}

void test_nil_lexical_binding_works() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* dynVal = newNum(35);
  newDynamicScope(sym, dynVal);
    newLexicalScope();
      addLexicalBinding(sym, nil);
        check_eq(lookup(sym), nil);
    endLexicalScope();
  endDynamicScope(sym);
  checkState();
}

void test_lexical_scopes_nest_correctly() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  Cell* val2 = newNum(35);
  check_eq(val->nrefs, 1);
  Cell* dynVal = newNum(36);
  check_eq(dynVal->nrefs, 1);
  newDynamicScope(sym, dynVal);
    check_eq(sym->nrefs, 2);
    check_eq(val->nrefs, 1);
    check_eq(val2->nrefs, 1);
    check_eq(dynVal->nrefs, 2);
    newLexicalScope();
      check(currLexicalScopes.top() != nil);
      check_eq(currLexicalScopes.top()->nrefs, 2);
      addLexicalBinding(sym, val);
        check_eq(lookup(sym), val);
        check_eq(sym->nrefs, 3);
        check_eq(val->nrefs, 2);
        check_eq(val2->nrefs, 1);
        check_eq(dynVal->nrefs, 2);
        newLexicalScope();
          check_eq(cdr(currLexicalScopes.top())->nrefs, 2);
          check_eq(currLexicalScopes.top()->nrefs, 2);
          addLexicalBinding(sym, val2);
            check_eq(lookup(sym), val2);
            check_eq(sym->nrefs, 4);
            check_eq(val->nrefs, 2);
            check_eq(val2->nrefs, 2);
            check_eq(dynVal->nrefs, 2);
        endLexicalScope();
      check_eq(currLexicalScopes.top()->nrefs, 2);
      check_eq(sym->nrefs, 3);
      check_eq(val->nrefs, 2);
      check_eq(val2->nrefs, 1);
      check_eq(dynVal->nrefs, 2);
    endLexicalScope();
    check_eq(lookup(sym), dynVal);
    check_eq(sym->nrefs, 2);
    check_eq(val->nrefs, 1);
    check_eq(val2->nrefs, 1);
    check_eq(dynVal->nrefs, 2);
  endDynamicScope(sym);
  check_eq(sym->nrefs, 1);
  check_eq(val->nrefs, 1);
  check_eq(val2->nrefs, 1);
  check_eq(dynVal->nrefs, 1);
  checkState();
}

void test_lower_lexical_scopes_are_available() {
  Cell* sym = newSym(L"a");
  check_eq(sym->nrefs, 1);
  Cell* val = newNum(34);
  check_eq(val->nrefs, 1);
  newLexicalScope();
    addLexicalBinding(sym, val);
      check_eq(lookup(sym), val);
      newLexicalScope();
        check_eq(lookup(sym), val);
      endLexicalScope();
  endLexicalScope();
  checkState();
}
