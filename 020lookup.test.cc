void test_Curr_lexical_scope_has_nil_cdr_on_startup() {
  CHECK_EQ(Curr_lexical_scopes.size(), 1);
  CHECK_EQ(cdr(Curr_lexical_scope), nil);
}

void test_lookup_returns_dynamic_binding() {
  TEMP(var, mkref(new_sym("a")));
  TEMP(val, mkref(new_num(34)));
  new_dynamic_scope(var, val);
    CHECK_EQ(lookup(var), val);
  end_dynamic_scope(var);
}

void test_lookup_returns_lexical_binding() {
  TEMP(var, mkref(new_sym("a")));
  TEMP(val, mkref(new_num(34)));
  new_lexical_scope();
    add_lexical_binding(var, val);
      CHECK_EQ(lookup(var), val);
  end_lexical_scope();
}

void test_lexical_binding_always_overrides_dynamic() {
  TEMP(var, mkref(new_sym("a")));
  TEMP(val, mkref(new_num(34)));
  TEMP(dyn_val, mkref(new_num(35)));
  new_dynamic_scope(var, dyn_val);
    new_lexical_scope();
      add_lexical_binding(var, val);
        CHECK_EQ(lookup(var), val);
    end_lexical_scope();

    CHECK_EQ(lookup(var), new_num(35));
  end_dynamic_scope(var);
}

void test_nil_lexical_binding_works() {
  TEMP(var, mkref(new_sym("a")));
  TEMP(dyn_val, mkref(new_num(35)));
  new_dynamic_scope(var, dyn_val);
    new_lexical_scope();
      add_lexical_binding(var, nil);
        CHECK_EQ(lookup(var), nil);
    end_lexical_scope();
  end_dynamic_scope(var);
}

void test_lexical_scopes_nest_correctly() {
  TEMP(var, mkref(new_sym("a")));
  TEMP(val, mkref(new_num(34)));
  TEMP(val2, mkref(new_num(35)));
  TEMP(dyn_val, mkref(new_num(36)));
  new_dynamic_scope(var, dyn_val);
    new_lexical_scope();
      CHECK(Curr_lexical_scope != nil);
      add_lexical_binding(var, val);
        CHECK_EQ(lookup(var), val);
        new_lexical_scope();
          add_lexical_binding(var, val2);
            CHECK_EQ(lookup(var), val2);
        end_lexical_scope();
    end_lexical_scope();
    CHECK_EQ(lookup(var), dyn_val);
  end_dynamic_scope(var);
}

void test_lower_lexical_scopes_are_available() {
  TEMP(var, mkref(new_sym("a")));
  TEMP(val, mkref(new_num(34)));
  new_lexical_scope();
    add_lexical_binding(var, val);
      CHECK_EQ(lookup(var), val);
      new_lexical_scope();
        CHECK_EQ(lookup(var), val);
      end_lexical_scope();
  end_lexical_scope();
}

void test_new_dynamic_scope_increments_refcounts() {
  TEMP(var, mkref(new_sym("a")));
  TEMP(val, mkref(new_num(34)));
  CLEAR_TRACE;
  new_dynamic_scope(var, val);
    CHECK_EQ(excess_mkrefs(), 2);  // one for var, one for val
  end_dynamic_scope(var);
}
