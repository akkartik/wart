void test_lexical_scope_has_nil_cdr_on_startup() {
  checkEq(currLexicalScopes.size(), 1);
  Cell* currLexicalScope = currLexicalScopes.top();
  checkEq(cdr(currLexicalScope), nil);
}

void test_lookup_returns_dynamic_binding() {
  Cell* sym = newSym("a");
  checkEq(sym->nrefs, 1);
  Cell* val = newNum(34);
  checkEq(val->nrefs, 1);
  newDynamicScope(sym, val);
    checkEq(lookup(sym), val);
    checkEq(sym->nrefs, 2);
    checkEq(val->nrefs, 2);
  endDynamicScope(sym);
  checkEq(sym->nrefs, 1);
  checkEq(val->nrefs, 1);
}

void test_lookup_returns_lexical_binding() {
  Cell* sym = newSym("a");
  checkEq(sym->nrefs, 1);
  Cell* val = newNum(34);
  checkEq(val->nrefs, 1);
  newLexicalScope();
    addLexicalBinding(sym, val);
      checkEq(lookup(sym), val);
      checkEq(sym->nrefs, 2);
      checkEq(val->nrefs, 2);
  endLexicalScope();
  checkEq(sym->nrefs, 1);
  checkEq(val->nrefs, 1);
}

void test_lexical_binding_always_overrides_dynamic() {
  Cell* sym = newSym("a");
  checkEq(sym->nrefs, 1);
  Cell* val = newNum(34);
  checkEq(val->nrefs, 1);
  Cell* dynVal = newNum(35);
  checkEq(dynVal->nrefs, 1);
  newDynamicScope(sym, dynVal);
    checkEq(sym->nrefs, 2);
    checkEq(val->nrefs, 1);
    checkEq(dynVal->nrefs, 2);
    newLexicalScope();
      addLexicalBinding(sym, val);
        checkEq(lookup(sym), val);
        checkEq(sym->nrefs, 3);
        checkEq(val->nrefs, 2);
        checkEq(dynVal->nrefs, 2);
    endLexicalScope();

    checkEq(lookup(sym), newNum(35));
    checkEq(sym->nrefs, 2);
    checkEq(val->nrefs, 1);
    checkEq(dynVal->nrefs, 2);
  endDynamicScope(sym);
  checkEq(sym->nrefs, 1);
  checkEq(val->nrefs, 1);
  checkEq(dynVal->nrefs, 1);
}

void test_nil_lexical_binding_works() {
  Cell* sym = newSym("a");
  checkEq(sym->nrefs, 1);
  Cell* dynVal = newNum(35);
  newDynamicScope(sym, dynVal);
    newLexicalScope();
      addLexicalBinding(sym, nil);
        checkEq(lookup(sym), nil);
    endLexicalScope();
  endDynamicScope(sym);
}

void test_lexical_scopes_nest_correctly() {
  Cell* sym = newSym("a");
  checkEq(sym->nrefs, 1);
  Cell* val = newNum(34);
  checkEq(val->nrefs, 1);
  Cell* val2 = newNum(35);
  checkEq(val->nrefs, 1);
  Cell* dynVal = newNum(36);
  checkEq(dynVal->nrefs, 1);
  newDynamicScope(sym, dynVal);
    checkEq(sym->nrefs, 2);
    checkEq(val->nrefs, 1);
    checkEq(val2->nrefs, 1);
    checkEq(dynVal->nrefs, 2);
    newLexicalScope();
      check(currLexicalScopes.top() != nil);
      checkEq(currLexicalScopes.top()->nrefs, 1);
      addLexicalBinding(sym, val);
        checkEq(lookup(sym), val);
        checkEq(sym->nrefs, 3);
        checkEq(val->nrefs, 2);
        checkEq(val2->nrefs, 1);
        checkEq(dynVal->nrefs, 2);
        newLexicalScope();
          checkEq(cdr(currLexicalScopes.top())->nrefs, 2);
          checkEq(currLexicalScopes.top()->nrefs, 1);
          addLexicalBinding(sym, val2);
            checkEq(lookup(sym), val2);
            checkEq(sym->nrefs, 4);
            checkEq(val->nrefs, 2);
            checkEq(val2->nrefs, 2);
            checkEq(dynVal->nrefs, 2);
        endLexicalScope();
      checkEq(currLexicalScopes.top()->nrefs, 1);
      checkEq(sym->nrefs, 3);
      checkEq(val->nrefs, 2);
      checkEq(val2->nrefs, 1);
      checkEq(dynVal->nrefs, 2);
    endLexicalScope();
    checkEq(lookup(sym), dynVal);
    checkEq(sym->nrefs, 2);
    checkEq(val->nrefs, 1);
    checkEq(val2->nrefs, 1);
    checkEq(dynVal->nrefs, 2);
  endDynamicScope(sym);
  checkEq(sym->nrefs, 1);
  checkEq(val->nrefs, 1);
  checkEq(val2->nrefs, 1);
  checkEq(dynVal->nrefs, 1);
}

void test_lower_lexical_scopes_are_available() {
  Cell* sym = newSym("a");
  checkEq(sym->nrefs, 1);
  Cell* val = newNum(34);
  checkEq(val->nrefs, 1);
  newLexicalScope();
    addLexicalBinding(sym, val);
      checkEq(lookup(sym), val);
      newLexicalScope();
        checkEq(lookup(sym), val);
      endLexicalScope();
  endLexicalScope();
}
