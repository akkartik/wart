void test_pointers_from_nil_are_nil() {
  checkEq(nil->car, nil);
  checkEq(nil->cdr, nil);
}

void test_newCell_has_nil_car_and_cdr() {
  Cell* x = newCell();
  checkEq(x->car, nil);
  checkEq(x->cdr, nil);
}

void test_Cell_layout_constraints() {
  Cell cell;
  check((sizeof(cell.car)%4) == 0);
  check((sizeof(cell.cdr)%4) == 0);
  check((sizeof(cell.type)%4) == 0);

  check(sizeof(long) <= sizeof(Cell*));
  check(sizeof(float) <= sizeof(Cell*));
  check(sizeof(size_t) <= sizeof(Cell*));
}
