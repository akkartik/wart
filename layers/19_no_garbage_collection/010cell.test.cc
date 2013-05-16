void test_pointers_from_nil_are_nil() {
  CHECK_EQ(nil->car, nil);
  CHECK_EQ(nil->cdr, nil);
}

void test_newCell_has_nil_car_and_cdr() {
  Cell* x = newCell();
  CHECK_EQ(x->car, nil);
  CHECK_EQ(x->cdr, nil);
}

void test_Cell_layout_constraints() {
  Cell cell;
  CHECK((sizeof(cell.car)%4) == 0);
  CHECK((sizeof(cell.cdr)%4) == 0);
  CHECK((sizeof(cell.type)%4) == 0);

  CHECK(sizeof(long) <= sizeof(Cell*));
  CHECK(sizeof(float) <= sizeof(Cell*));
  CHECK(sizeof(size_t) <= sizeof(Cell*));
}
