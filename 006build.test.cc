// check all nrefs except quotes/unquotes

void test_build_handles_empty_input() {
  list<Cell*> cells = buildFromStream(stream(L""));
  check(cells.empty());
}

void test_build_handles_nil() {
  CodeStream cs(stream(L"()"));
  checkEq(nextRawCell(cs), nil);
  check(eof(cs.fd));
}

void test_build_handles_nil2() {
  CodeStream cs(stream(L"nil"));
  checkEq(nextRawCell(cs), nil);
  check(eof(cs.fd));
}

void test_build_handles_number() {
  CodeStream cs(stream(L"34"));
  Cell* c = nextRawCell(cs);
  checkEq(c, newNum(34));
  checkEq(c->nrefs, 1);
  rmref(c);
  check(eof(cs.fd));
}

void test_build_handles_sym() {
  CodeStream cs(stream(L"a"));
  Cell* c = nextRawCell(cs);
  checkEq(c, newSym(L"a"));
  checkEq(c->nrefs, 1);
  rmref(c);
  check(eof(cs.fd));
}

void test_build_doesnt_mix_syms_and_strings() {
  Cell* s = newString(L"a");
  check(s != newSym(L"a"));
  rmref(s);
}

void test_build_handles_quoted_sym() {
  CodeStream cs(stream(L"'a"));
  Cell* c = nextRawCell(cs);
  checkEq(car(c), newSym(L"'"));
  checkEq(cdr(c), newSym(L"a"));
  checkEq(cdr(c)->nrefs, 2);
  rmref(c);
  check(eof(cs.fd));
}

void test_build_handles_nested_quote() {
  CodeStream cs(stream(L"',a"));
  Cell* c = nextRawCell(cs);
  checkEq(car(c), newSym(L"'"));
  checkEq(car(cdr(c)), newSym(L","));
  checkEq(cdr(cdr(c)), newSym(L"a"));
  checkEq(cdr(cdr(c))->nrefs, 2);
  rmref(c);
  check(eof(cs.fd));
}

void test_build_handles_multiple_atoms() {
  CodeStream cs(stream(L"34\n35"));
  Cell* c = nextRawCell(cs);
  checkEq(c, newNum(34));
  checkEq(c->nrefs, 1);
  checkEq(cdr(c), nil);

  c = nextRawCell(cs);
  checkEq(c, newNum(35));
  checkEq(c->nrefs, 1);
  checkEq(cdr(c), nil);

  check(eof(cs.fd));
}

void test_build_handles_form() {
  CodeStream cs(stream(L"34 35"));
  Cell *c=nextRawCell(cs), *origc=c;
  checkEq(c->nrefs, 0);
  checkEq(car(c), newNum(34));
  checkEq(car(c)->nrefs, 2);

  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newNum(35));
  checkEq(car(c)->nrefs, 2);

  checkEq(cdr(c), nil);
  rmref(origc);
  check(eof(cs.fd));
}

void test_build_handles_dot() {
  CodeStream cs(stream(L"34 . 35"));
  Cell *c=nextRawCell(cs), *origc=c;
  checkEq(c->nrefs, 0);
  checkEq(car(c), newNum(34));
  checkEq(car(c)->nrefs, 2);

  c = cdr(c);
  checkEq(c, newNum(35));
  checkEq(c->nrefs, 2);

  rmref(origc);
  check(eof(cs.fd));
}

void test_build_handles_nested_form() {
  CodeStream cs(stream(L"(3 7 (33 23))"));
  Cell *c=nextRawCell(cs), *origc=c;
  checkEq(c->nrefs, 0);
  checkEq(car(c), newNum(3));
  checkEq(car(c)->nrefs, 2);

  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newNum(7));
  checkEq(car(c)->nrefs, 2);

  c = cdr(c);
  checkEq(c->nrefs, 1);
    Cell* c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newNum(33));
    checkEq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newNum(23));
    checkEq(car(c2)->nrefs, 2);
    checkEq(cdr(c2), nil);
  checkEq(cdr(c), nil);

  rmref(origc);
  check(eof(cs.fd));
}

void test_build_handles_strings() {
  CodeStream cs(stream(L"(3 7 (33 \"abc\" 23))"));
  Cell *c=nextRawCell(cs), *origc=c;
  checkEq(c->nrefs, 0);
  checkEq(car(c), newNum(3));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newNum(7));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
  checkEq(c->nrefs, 1);
    Cell* c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newNum(33));
    checkEq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    checkEq(c2->nrefs, 1);
    check(isString(car(c2)));
    checkEq(toString(car(c2)), L"abc");
    checkEq(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newNum(23));
    checkEq(car(c2)->nrefs, 2);
    checkEq(cdr(c2), nil);
  checkEq(cdr(c), nil);

  rmref(origc);
  check(eof(cs.fd));
}

void test_build_handles_syms() {
  CodeStream cs(stream(L"(3 7 (33 \"abc\" 3de 23))"));
  Cell *c=nextRawCell(cs), *origc=c;
  checkEq(c->nrefs, 0);
  checkEq(car(c), newNum(3));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newNum(7));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
    Cell* c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newNum(33));
    checkEq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    checkEq(c2->nrefs, 1);
    check(isString(car(c2)));
    checkEq(toString(car(c2)), L"abc");
    checkEq(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym(L"3de"));
    checkEq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newNum(23));
    checkEq(car(c2)->nrefs, 2);
    checkEq(cdr(c2), nil);
  checkEq(cdr(c), nil);

  rmref(origc);
  check(eof(cs.fd));
}

void test_build_handles_quotes() {
  CodeStream cs(stream(L"`(34 ,(35) ,36 ,@37 @,38 @39 ,'(a))"));
  Cell *c=nextRawCell(cs), *origc=c;
  checkEq(c->nrefs, 0);
  checkEq(car(c), newSym(L"`"));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newNum(34));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
    Cell* c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym(L","));
    c2 = cdr(c2);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newNum(35));
    checkEq(car(c2)->nrefs, 2);
    checkEq(cdr(c2), nil);
  c = cdr(c);
    c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym(L","));
    checkEq(cdr(c2), newNum(36));
    checkEq(cdr(c2)->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym(L",@"));
    checkEq(cdr(c2), newNum(37));
    checkEq(cdr(c2)->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym(L"@"));
    checkEq(car(cdr(c2)), newSym(L","));
    checkEq(cdr(cdr(c2)), newNum(38));
    checkEq(cdr(cdr(c2))->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym(L"@"));
    checkEq(cdr(c2), newNum(39));
    checkEq(cdr(c2)->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym(L","));
    checkEq(cdr(c2)->nrefs, 1);
    c2 = cdr(c2);
    checkEq(car(c2), newSym(L"'"));
    c2 = cdr(c2);
    checkEq(car(c2), newSym(L"a"));
    checkEq(car(c2)->nrefs, 2);
    checkEq(cdr(c2), nil);
  checkEq(cdr(c), nil);

  rmref(origc);
  check(eof(cs.fd));
}
