// check all nrefs except quotes/unquotes

void test_build_handles_empty_input() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"")))));
  check(cells.empty());
}

void test_build_handles_nil() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"()")))));
  check_eq(cells.front(), nil);
}

void test_build_handles_nil2() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"nil")))));
  check_eq(cells.front(), nil);
}

void test_build_handles_number() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"34")))));
  check_eq(cells.size(), 1);
  check_eq(cells.front(), newNum(34));
  check_eq(cells.front()->nrefs, 1);
}

void test_build_handles_sym() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"a")))));
  check_eq(cells.size(), 1);
  check_eq(cells.front(), newSym(L"a"));
  check_eq(cells.front()->nrefs, 1);
}

void test_build_doesnt_mix_syms_and_strings() {
  Cell* s = newString(L"a");
  check(s != newSym(L"a"));
  rmref(s);
}

void test_build_handles_quoted_sym() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"'a")))));
  check_eq(cells.size(), 1);
  check_eq(car(cells.front()), newSym(L"'"));
  check_eq(cdr(cells.front()), newSym(L"a"));
  check_eq(cdr(cells.front())->nrefs, 2);
  rmref(cells.front());
}

void test_build_handles_nested_quote() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"',a")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check_eq(car(c), newSym(L"'"));
  check_eq(car(cdr(c)), newSym(L","));
  check_eq(cdr(cdr(c)), newSym(L"a"));
  check_eq(cdr(cdr(c))->nrefs, 2);
  rmref(cells.front());
}

void test_build_handles_multiple_atoms() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"34\n35")))));
  check_eq(cells.size(), 2);
  Cell* c = cells.front();
  check_eq(c, newNum(34));
  check_eq(c->nrefs, 1);
  check_eq(cdr(c), nil);

  c = cells.back();
  check_eq(c, newNum(35));
  check_eq(c->nrefs, 1);
  check_eq(cdr(c), nil);

}

void test_build_handles_form() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"34 35")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check_eq(c->nrefs, 0);
  check_eq(car(c), newNum(34));
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check_eq(c->nrefs, 1);
  check_eq(car(c), newNum(35));
  check_eq(car(c)->nrefs, 2);

  check_eq(cdr(c), nil);
  rmref(cells.front());
}

void test_build_handles_dot() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"34 . 35")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check_eq(c->nrefs, 0);
  check_eq(car(c), newNum(34));
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check_eq(c, newNum(35));
  check_eq(c->nrefs, 2);

  rmref(cells.front());
}

void test_build_handles_nested_form() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"(3 7 (33 23))")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check_eq(c->nrefs, 0);
  check_eq(car(c), newNum(3));
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check_eq(c->nrefs, 1);
  check_eq(car(c), newNum(7));
  check_eq(car(c)->nrefs, 2);

  c = cdr(c);
  check_eq(c->nrefs, 1);
    Cell* c2 = car(c);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newNum(33));
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newNum(23));
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
}

void test_build_handles_strings() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"(3 7 (33 \"abc\" 23))")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check_eq(c->nrefs, 0);
  check_eq(car(c), newNum(3));
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check_eq(c->nrefs, 1);
  check_eq(car(c), newNum(7));
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check_eq(c->nrefs, 1);
    Cell* c2 = car(c);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newNum(33));
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check_eq(c2->nrefs, 1);
    check(isString(car(c2)));
    check_eq(toString(car(c2)), L"abc");
    check_eq(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newNum(23));
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
}

void test_build_handles_syms() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"(3 7 (33 \"abc\" 3de 23))")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check_eq(c->nrefs, 0);
  check_eq(car(c), newNum(3));
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check_eq(c->nrefs, 1);
  check_eq(car(c), newNum(7));
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
    Cell* c2 = car(c);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newNum(33));
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check_eq(c2->nrefs, 1);
    check(isString(car(c2)));
    check_eq(toString(car(c2)), L"abc");
    check_eq(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newSym(L"3de"));
    check_eq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newNum(23));
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
}

void test_build_handles_quotes() {
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(stream(L"`(34 ,(35) ,36 ,@37 ,'(a))")))));
  check_eq(cells.size(), 1);
  Cell* c = cells.front();
  check_eq(c->nrefs, 0);
  check_eq(car(c), newSym(L"`"));
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
  check_eq(c->nrefs, 1);
  check_eq(car(c), newNum(34));
  check_eq(car(c)->nrefs, 2);
  c = cdr(c);
    Cell* c2 = car(c);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newSym(L","));
    c2 = cdr(c2);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newNum(35));
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  c = cdr(c);
    c2 = car(c);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newSym(L","));
    check_eq(cdr(c2), newNum(36));
    check_eq(cdr(c2)->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newSym(L",@"));
    check_eq(cdr(c2), newNum(37));
    check_eq(cdr(c2)->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    check_eq(c2->nrefs, 1);
    check_eq(car(c2), newSym(L","));
    check_eq(cdr(c2)->nrefs, 1);
    c2 = cdr(c2);
    check_eq(car(c2), newSym(L"'"));
    c2 = cdr(c2);
    check_eq(car(c2), newSym(L"a"));
    check_eq(car(c2)->nrefs, 2);
    check_eq(cdr(c2), nil);
  check_eq(cdr(c), nil);

  rmref(cells.front());
}
