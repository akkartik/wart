void test_pointers_from_nil_are_nil() {
  CHECK_EQ(nil->car, nil);
  CHECK_EQ(nil->cdr, nil);
}

void test_new_cell_has_nil_car_and_cdr() {
  cell* x = new_cell();
  CHECK_EQ(x->car, nil);
  CHECK_EQ(x->cdr, nil);
  rmref(x);
}

void test_rmref_frees_space() {
  cell* c = new_cell();
  CHECK_EQ(c->car, nil);
  CHECK_EQ(Free_cells, NULL);
  rmref(c);
  CHECK(!c->car);
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
