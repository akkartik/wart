void test_pointers_from_nil_are_nil() {
  CHECK_EQ(nil->car, nil);
  CHECK_EQ(nil->cdr, nil);
}

void test_newCell_has_nil_car_and_cdr() {
  Cell* x = newCell();
  CHECK_EQ(x->car, nil);
  CHECK_EQ(x->cdr, nil);
  rmref(x);
}

void test_rmref_frees_space() {
  Cell* c = newCell();
  CHECK_EQ(c->car, nil);
  CHECK_EQ(freelist, NULL);
  rmref(c);
  CHECK(!c->car);
  CHECK_EQ(freelist, c);
}

void test_rmref_handles_nums() {
  Cell* c = newNum(34);
  rmref(c);
  CHECK(!c->car);
  CHECK_EQ(freelist, c);
}

void test_Cell_layout_constraints() {
  Cell cell;
  CHECK((sizeof(cell.car)%4) == 0);
  CHECK((sizeof(cell.cdr)%4) == 0);
  CHECK((sizeof(cell.type)%4) == 0);
  CHECK((sizeof(cell.nrefs)%4) == 0);

  CHECK(sizeof(long) <= sizeof(Cell*));
  CHECK(sizeof(float) <= sizeof(Cell*));
  CHECK(sizeof(size_t) <= sizeof(Cell*));
}
