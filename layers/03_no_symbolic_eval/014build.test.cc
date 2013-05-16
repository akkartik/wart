// CHECK all nrefs except quotes/unquotes

void test_build_handles_nil() {
  IndentSensitiveStream in("()");
  CHECK_EQ(nextCell(in), nil);
}

void test_build_handles_nil2() {
  IndentSensitiveStream in("nil");
  CHECK_EQ(nextCell(in), nil);
}

void test_build_handles_integer() {
  IndentSensitiveStream in("34");
  Cell* c = nextCell(in);
  CHECK_EQ(c, newNum(34));
  CHECK_EQ(c->nrefs, 1);
  rmref(c);
}

void test_build_handles_float() {
  IndentSensitiveStream in("3.4");
  Cell* c = nextCell(in);
  CHECK(isNum(c));
  CHECK(equalFloats(toFloat(c), 3.4));
  CHECK_EQ(c->nrefs, 0);   // floats aren't interned
  rmref(c);
}

void test_build_warns_on_ambiguous_float() {
  IndentSensitiveStream in("-.4");
  Cell* c = nextCell(in);
  CHECK_EQ(raiseCount, 1); raiseCount=0;
  CHECK(isNum(c));
  CHECK(equalFloats(toFloat(c), -0.4));
  rmref(c);
}

void test_build_creates_floats_on_overflow() {
  IndentSensitiveStream in("100000000000000000000");
  Cell* c = nextCell(in);
  CHECK_EQ(raiseCount, 1); raiseCount=0;   // overflow warning
  CHECK_EQ(c->type, FLOAT);
  CHECK_EQ(c->nrefs, 0);
  rmref(c);
}

void test_build_handles_sym() {
  IndentSensitiveStream in("a");
  Cell* c = nextCell(in);
  CHECK_EQ(c, newSym("a"));
  CHECK_EQ(c->nrefs, 1);
  rmref(c);
}

void test_build_handles_string() {
  IndentSensitiveStream in("\"a\"");
  Cell* c = nextCell(in);
  CHECK_EQ(toString(c), "a");
  CHECK_EQ(c->nrefs, 0);   // strings aren't interned
  rmref(c);
}

void test_build_doesnt_mix_syms_and_strings() {
  Cell* s = newString("a");
  CHECK(s != newSym("a"));
  rmref(s);
}

void test_build_handles_quoted_sym() {
  IndentSensitiveStream in("'a");
  Cell* c = nextCell(in);
  CHECK_EQ(car(c), newSym("'"));
  CHECK_EQ(cdr(c), newSym("a"));
  CHECK_EQ(cdr(c)->nrefs, 2);
  rmref(c);
}

void test_build_handles_nested_quote() {
  IndentSensitiveStream in("',a");
  Cell* c = nextCell(in);
  CHECK_EQ(car(c), newSym("'"));
  CHECK_EQ(car(cdr(c)), newSym(","));
  CHECK_EQ(cdr(cdr(c)), newSym("a"));
  CHECK_EQ(cdr(cdr(c))->nrefs, 2);
  rmref(c);
}

void test_build_handles_multiple_atoms() {
  IndentSensitiveStream in("34\n35");
  Cell* c = nextCell(in);
  CHECK_EQ(c, newNum(34));
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(cdr(c), nil);

  c = nextCell(in);
  CHECK_EQ(c, newNum(35));
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(cdr(c), nil);
}

void test_build_handles_form() {
  IndentSensitiveStream in("(34 35)");
  Cell *c=nextCell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), newNum(34));
  CHECK_EQ(car(c)->nrefs, 2);

  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), newNum(35));
  CHECK_EQ(car(c)->nrefs, 2);

  CHECK_EQ(cdr(c), nil);
  rmref(origc);
}

void test_build_handles_dotted_list() {
  IndentSensitiveStream in("(34 ... 35)");
  Cell *c=nextCell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), newNum(34));
  CHECK_EQ(car(c)->nrefs, 2);

  c = cdr(c);
  CHECK_EQ(c, newNum(35));
  CHECK_EQ(c->nrefs, 2);

  rmref(origc);
}

void test_build_handles_literal_ellipses() {
  IndentSensitiveStream in("'...");
  Cell *c=nextCell(in);
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), newSym("'"));
  CHECK_EQ(cdr(c), newSym("..."));
  CHECK_EQ(cdr(c)->nrefs, 2);
  rmref(c);
}

void test_build_handles_nested_form() {
  IndentSensitiveStream in("(3 7 (33 23))");
  Cell *c=nextCell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), newNum(3));
  CHECK_EQ(car(c)->nrefs, 2);

  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), newNum(7));
  CHECK_EQ(car(c)->nrefs, 2);

  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
    Cell* c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newNum(33));
    CHECK_EQ(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newNum(23));
    CHECK_EQ(car(c2)->nrefs, 2);
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);

  rmref(origc);
}

void test_build_handles_strings() {
  IndentSensitiveStream in("(3 7 (33 \"abc\" 23))");
  Cell *c=nextCell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), newNum(3));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), newNum(7));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
    Cell* c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newNum(33));
    CHECK_EQ(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK(isString(car(c2)));
    CHECK_EQ(toString(car(c2)), "abc");
    CHECK_EQ(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newNum(23));
    CHECK_EQ(car(c2)->nrefs, 2);
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);

  rmref(origc);
}

void test_build_handles_syms() {
  IndentSensitiveStream in("(3 7 (33 \"abc\" 3de 23))");
  Cell *c=nextCell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), newNum(3));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), newNum(7));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
    Cell* c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newNum(33));
    CHECK_EQ(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK(isString(car(c2)));
    CHECK_EQ(toString(car(c2)), "abc");
    CHECK_EQ(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newSym("3de"));
    CHECK_EQ(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newNum(23));
    CHECK_EQ(car(c2)->nrefs, 2);
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);

  rmref(origc);
}

void test_build_handles_quotes() {
  IndentSensitiveStream in("`(34 ,(35) ,36 ,@37 @,38 @39 ,'(a))");
  Cell *c=nextCell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), newSym("`"));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), newNum(34));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
    Cell* c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newSym(","));
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newNum(35));
    CHECK_EQ(car(c2)->nrefs, 2);
    CHECK_EQ(cdr(c2), nil);
  c = cdr(c);
    c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newSym(","));
    CHECK_EQ(cdr(c2), newNum(36));
    CHECK_EQ(cdr(c2)->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newSym(",@"));
    CHECK_EQ(cdr(c2), newNum(37));
    CHECK_EQ(cdr(c2)->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newSym("@"));
    CHECK_EQ(car(cdr(c2)), newSym(","));
    CHECK_EQ(cdr(cdr(c2)), newNum(38));
    CHECK_EQ(cdr(cdr(c2))->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newSym("@"));
    CHECK_EQ(cdr(c2), newNum(39));
    CHECK_EQ(cdr(c2)->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), newSym(","));
    CHECK_EQ(cdr(c2)->nrefs, 1);
    c2 = cdr(c2);
    CHECK_EQ(car(c2), newSym("'"));
    c2 = cdr(c2);
    CHECK_EQ(car(c2), newSym("a"));
    CHECK_EQ(car(c2)->nrefs, 2);
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);

  rmref(origc);
}

void test_build_handles_indented_wrapped_lines() {
  IndentSensitiveStream in("a\n  (a b c\n   d e)");
  Cell *c0=nextCell(in);
  CHECK_EQ(c0, newSym("a"));
  CHECK_EQ(c0->nrefs, 1);

  Cell *c=nextCell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), newSym("a"));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), newSym("b"));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), newSym("c"));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), newSym("d"));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), newSym("e"));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c, nil);
  rmref(origc);
  rmref(c0);
}
