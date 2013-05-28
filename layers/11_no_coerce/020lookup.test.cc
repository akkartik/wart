void test_lexical_scope_has_nil_cdr_on_startup() {
  CHECK_EQ(Curr_lexical_scopes.size(), 1);
  CHECK_EQ(cdr(Curr_lexical_scope), nil);
}

void test_lookup_returns_dynamic_binding() {
  cell* sym = new_sym("a");
  CHECK_EQ(sym->nrefs, 1);
  cell* val = new_num(34);
  CHECK_EQ(val->nrefs, 1);
  new_dynamic_scope(sym, val);
    CHECK_EQ(lookup(sym), val);
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 2);
  end_dynamic_scope(sym);
  CHECK_EQ(sym->nrefs, 1);
  CHECK_EQ(val->nrefs, 1);
}

void test_lookup_returns_lexical_binding() {
  cell* sym = new_sym("a");
  CHECK_EQ(sym->nrefs, 1);
  cell* val = new_num(34);
  CHECK_EQ(val->nrefs, 1);
  new_lexical_scope();
    add_lexical_binding(sym, val);
      CHECK_EQ(lookup(sym), val);
      CHECK_EQ(sym->nrefs, 2);
      CHECK_EQ(val->nrefs, 2);
  end_lexical_scope();
  CHECK_EQ(sym->nrefs, 1);
  CHECK_EQ(val->nrefs, 1);
}

void test_lexical_binding_always_overrides_dynamic() {
  cell* sym = new_sym("a");
  CHECK_EQ(sym->nrefs, 1);
  cell* val = new_num(34);
  CHECK_EQ(val->nrefs, 1);
  cell* dyn_val = new_num(35);
  CHECK_EQ(dyn_val->nrefs, 1);
  new_dynamic_scope(sym, dyn_val);
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 1);
    CHECK_EQ(dyn_val->nrefs, 2);
    new_lexical_scope();
      add_lexical_binding(sym, val);
        CHECK_EQ(lookup(sym), val);
        CHECK_EQ(sym->nrefs, 3);
        CHECK_EQ(val->nrefs, 2);
        CHECK_EQ(dyn_val->nrefs, 2);
    end_lexical_scope();

    CHECK_EQ(lookup(sym), new_num(35));
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 1);
    CHECK_EQ(dyn_val->nrefs, 2);
  end_dynamic_scope(sym);
  CHECK_EQ(sym->nrefs, 1);
  CHECK_EQ(val->nrefs, 1);
  CHECK_EQ(dyn_val->nrefs, 1);
}

void test_nil_lexical_binding_works() {
  cell* sym = new_sym("a");
  CHECK_EQ(sym->nrefs, 1);
  cell* dyn_val = new_num(35);
  new_dynamic_scope(sym, dyn_val);
    new_lexical_scope();
      add_lexical_binding(sym, nil);
        CHECK_EQ(lookup(sym), nil);
    end_lexical_scope();
  end_dynamic_scope(sym);
}

void test_lexical_scopes_nest_correctly() {
  cell* sym = new_sym("a");
  CHECK_EQ(sym->nrefs, 1);
  cell* val = new_num(34);
  CHECK_EQ(val->nrefs, 1);
  cell* val2 = new_num(35);
  CHECK_EQ(val->nrefs, 1);
  cell* dyn_val = new_num(36);
  CHECK_EQ(dyn_val->nrefs, 1);
  new_dynamic_scope(sym, dyn_val);
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 1);
    CHECK_EQ(val2->nrefs, 1);
    CHECK_EQ(dyn_val->nrefs, 2);
    new_lexical_scope();
      CHECK(Curr_lexical_scope != nil);
      CHECK_EQ(Curr_lexical_scope->nrefs, 1);
      add_lexical_binding(sym, val);
        CHECK_EQ(lookup(sym), val);
        CHECK_EQ(sym->nrefs, 3);
        CHECK_EQ(val->nrefs, 2);
        CHECK_EQ(val2->nrefs, 1);
        CHECK_EQ(dyn_val->nrefs, 2);
        new_lexical_scope();
          CHECK_EQ(cdr(Curr_lexical_scope)->nrefs, 2);
          CHECK_EQ(Curr_lexical_scope->nrefs, 1);
          add_lexical_binding(sym, val2);
            CHECK_EQ(lookup(sym), val2);
            CHECK_EQ(sym->nrefs, 4);
            CHECK_EQ(val->nrefs, 2);
            CHECK_EQ(val2->nrefs, 2);
            CHECK_EQ(dyn_val->nrefs, 2);
        end_lexical_scope();
      CHECK_EQ(Curr_lexical_scope->nrefs, 1);
      CHECK_EQ(sym->nrefs, 3);
      CHECK_EQ(val->nrefs, 2);
      CHECK_EQ(val2->nrefs, 1);
      CHECK_EQ(dyn_val->nrefs, 2);
    end_lexical_scope();
    CHECK_EQ(lookup(sym), dyn_val);
    CHECK_EQ(sym->nrefs, 2);
    CHECK_EQ(val->nrefs, 1);
    CHECK_EQ(val2->nrefs, 1);
    CHECK_EQ(dyn_val->nrefs, 2);
  end_dynamic_scope(sym);
  CHECK_EQ(sym->nrefs, 1);
  CHECK_EQ(val->nrefs, 1);
  CHECK_EQ(val2->nrefs, 1);
  CHECK_EQ(dyn_val->nrefs, 1);
}

void test_lower_lexical_scopes_are_available() {
  cell* sym = new_sym("a");
  CHECK_EQ(sym->nrefs, 1);
  cell* val = new_num(34);
  CHECK_EQ(val->nrefs, 1);
  new_lexical_scope();
    add_lexical_binding(sym, val);
      CHECK_EQ(lookup(sym), val);
      new_lexical_scope();
        CHECK_EQ(lookup(sym), val);
      end_lexical_scope();
  end_lexical_scope();
}
