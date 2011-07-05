void test_build_handles_empty_input() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"")))));
  check(cells.empty());
  checkState();
}

void test_build_handles_nil() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"()")))));
  check_eq(cells.front(), nil);
  checkState();
}

void test_build_handles_nil2() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"nil")))));
  check_eq(cells.front(), nil);
  checkState();
}

void test_build_handles_number() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"34")))));
  check_eq(cells.size(), 1);
  check(isNum(cells.front()));
  check_eq(toNum(cells.front()), 34);
  check_eq(cells.front()->nrefs, 1);
  checkState();
}

void test_build_handles_symbol() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"a")))));
  check_eq(cells.size(), 1);
  check(isSym(cells.front()));
  check_eq(toString(cells.front()), L"a");
  check_eq(cells.front()->nrefs, 1);
  checkState();
}

void test_build_doesnt_mix_symbols_and_strings() {
  Cell* s = newString(L"a");
  check(s != newSym(L"a"));
  rmref(s);
  checkState();
}

void test_build_handles_quoted_symbol() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"'a")))));
  check_eq(cells.size(), 1);
  check(isCons(cells.front()));
  check(isSym(car(cells.front())));
  check_eq(toString(car(cells.front())), L"'");
  // nrefs of quote depends on number of compiled functions with quoted params
  check(isSym(car(cells.front())));
  check_eq(toString(cdr(cells.front())), L"a");
  check_eq(cdr(cells.front())->nrefs, 2);
  rmref(cells.front());
  checkState();
}

void test_build_handles_multiple_atoms() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"34\n35")))));
  check_eq(cells.size(), 2);
  Cell* c = cells.front();
  check(isNum(c));
  check_eq(toNum(c), 34);
  check_eq(c->nrefs, 1);
  check_eq(cdr(c), nil);

  c = cells.back();
  check(isNum(c));
  check_eq(toNum(c), 35);
  check_eq(c->nrefs, 1);
  check_eq(cdr(c), nil);

  checkState();
}

void test_build_handles_form() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"34 35")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 34);
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 35);
  check_eq(car(c)->nrefs, 2);
  check_eq(cdr(c), nil);

  rmref(cells.front());
  checkState();
}

void test_build_handles_dot() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"34 . 35")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 34);
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check(isNum(c));
  check_eq(toNum(c), 35);
  check_eq(c->nrefs, 2);

  rmref(cells.front());
  checkState();
}

void test_build_handles_nested_form() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"(3 7 (33 23))")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 3);
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 7);
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
    Cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 33);
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 23);
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
  checkState();
}

void test_build_handles_strings() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"(3 7 (33 \"abc\" 23))")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 3);
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 7);
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
    Cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 33);
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isString(car(c2)));
    check_eq(toString(car(c2)), L"abc");
    check_eq(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 23);
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
  checkState();
}

void test_build_handles_syms() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"(3 7 (33 \"abc\" 3de 23))")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 3);
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 7);
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
    Cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 33);
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isString(car(c2)));
    check_eq(toString(car(c2)), L"abc");
    check_eq(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isSym(car(c2)));
    check_eq(toString(car(c2)), L"3de");
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 23);
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
  checkState();
}

void test_build_handles_quotes() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(teststream(L"`(34 ,35)")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check(isCons(c));
  check_eq(c->nrefs, 0);
  check(isSym(car(c)));
  check_eq(toString(car(c)), L"`");
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
  check_eq(c->nrefs, 1);
  check(isNum(car(c)));
  check_eq(toNum(car(c)), 34);
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check(isCons(c));
    Cell* c2 = car(c);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isSym(car(c2)));
    check_eq(toString(car(c2)), L",");
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check(isCons(c2));
    check_eq(c2->nrefs, 1);
    check(isNum(car(c2)));
    check_eq(toNum(car(c2)), 35);
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
  checkState();
}
