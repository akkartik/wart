void test_pointers_from_nil_are_nil() {
  CHECK_EQ(nil->car, nil);
  CHECK_EQ(nil->cdr, nil);
}

void test_new_cell_has_nil_car_and_cdr() {
  cell* x = new_cell();
  CHECK_EQ(x->car, nil);
  CHECK_EQ(x->cdr, nil);
}

void test_new_cell_doesnt_mkref() {
  CLEAR_TRACE;
  new_cell();
  CHECK_EQ(trace_count("gc", "alloc"), 1);
  CHECK_TRACE_DOESNT_CONTAIN("gc", "mkref");
}

void test_rmref_frees_space() {
  CLEAR_TRACE;
  cell* c = new_cell();
  rmref(c);
  CHECK_EQ(trace_count("gc", "free"), 1);
  CHECK_EQ(Free_cells, c);
}

void test_rmref_doesnt_free_interned() {
  cell* c = new_num(34);
  rmref(c);
  CHECK_EQ(trace_count("gc", "free"), 1);
  CHECK_EQ(Free_cells, c);
}

void test_cell_layout_constraints() {
  cell c;
  CHECK((sizeof(c.car)%4) == 0);
  CHECK((sizeof(c.cdr)%4) == 0);
  CHECK((sizeof(c.type)%4) == 0);
  CHECK((sizeof(c.nrefs)%4) == 0);

  CHECK(sizeof(long) <= sizeof(cell*));
  CHECK(sizeof(float) <= sizeof(cell*));
  CHECK(sizeof(size_t) <= sizeof(cell*));
}
