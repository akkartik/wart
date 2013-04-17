// check all nrefs except quotes/unquotes

void test_build_handles_nil() {
  IndentSensitiveStream in("()");
  checkEq(nextCell(in), nil);
}

void test_build_handles_nil2() {
  IndentSensitiveStream in("nil");
  checkEq(nextCell(in), nil);
}

void test_build_handles_integer() {
  IndentSensitiveStream in("34");
  Cell* c = nextCell(in);
  checkEq(c, newNum(34));
  checkEq(c->nrefs, 1);
  rmref(c);
}

void test_build_handles_float() {
  IndentSensitiveStream in("3.4");
  Cell* c = nextCell(in);
  check(isNum(c));
  check(equalFloats(toFloat(c), 3.4));
  checkEq(c->nrefs, 0);   // floats aren't interned
  rmref(c);
}

void test_build_warns_on_ambiguous_float() {
  IndentSensitiveStream in("-.4");
  Cell* c = nextCell(in);
  checkEq(raiseCount, 1); raiseCount=0;
  check(isNum(c));
  check(equalFloats(toFloat(c), -0.4));
  rmref(c);
}

void test_build_creates_floats_on_overflow() {
  IndentSensitiveStream in("100000000000000000000");
  Cell* c = nextCell(in);
  checkEq(raiseCount, 1); raiseCount=0;   // overflow warning
  checkEq(c->type, FLOAT);
  checkEq(c->nrefs, 0);
  rmref(c);
}

void test_build_handles_sym() {
  IndentSensitiveStream in("a");
  Cell* c = nextCell(in);
  checkEq(c, newSym("a"));
  checkEq(c->nrefs, 1);
  rmref(c);
}

void test_build_handles_string() {
  IndentSensitiveStream in("\"a\"");
  Cell* c = nextCell(in);
  checkEq(toString(c), "a");
  checkEq(c->nrefs, 0);   // strings aren't interned
  rmref(c);
}

void test_build_doesnt_mix_syms_and_strings() {
  Cell* s = newString("a");
  check(s != newSym("a"));
  rmref(s);
}

void test_build_handles_quoted_sym() {
  IndentSensitiveStream in("'a");
  Cell* c = nextCell(in);
  checkEq(car(c), newSym("'"));
  checkEq(cdr(c), newSym("a"));
  checkEq(cdr(c)->nrefs, 2);
  rmref(c);
}

void test_build_handles_nested_quote() {
  IndentSensitiveStream in("',a");
  Cell* c = nextCell(in);
  checkEq(car(c), newSym("'"));
  checkEq(car(cdr(c)), newSym(","));
  checkEq(cdr(cdr(c)), newSym("a"));
  checkEq(cdr(cdr(c))->nrefs, 2);
  rmref(c);
}

void test_build_handles_multiple_atoms() {
  IndentSensitiveStream in("34\n35");
  Cell* c = nextCell(in);
  checkEq(c, newNum(34));
  checkEq(c->nrefs, 1);
  checkEq(cdr(c), nil);

  c = nextCell(in);
  checkEq(c, newNum(35));
  checkEq(c->nrefs, 1);
  checkEq(cdr(c), nil);
}

void test_build_handles_form() {
  IndentSensitiveStream in("(34 35)");
  Cell *c=nextCell(in), *origc=c;
  checkEq(c->nrefs, 0);
  checkEq(car(c), newNum(34));
  checkEq(car(c)->nrefs, 2);

  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newNum(35));
  checkEq(car(c)->nrefs, 2);

  checkEq(cdr(c), nil);
  rmref(origc);
}

void test_build_handles_metadata() {
  IndentSensitiveStream in(":(34 35)");
  Cell *c=nextCell(in), *origc=c;
  checkEq(c->nrefs, 0);
  checkEq(car(c), newSym(":"));

  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newNum(34));
  checkEq(car(c)->nrefs, 2);

  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newNum(35));
  checkEq(car(c)->nrefs, 2);

  checkEq(cdr(c), nil);
  rmref(origc);
}

void test_build_handles_dotted_list() {
  IndentSensitiveStream in("(34 ... 35)");
  Cell *c=nextCell(in), *origc=c;
  checkEq(c->nrefs, 0);
  checkEq(car(c), newNum(34));
  checkEq(car(c)->nrefs, 2);

  c = cdr(c);
  checkEq(c, newNum(35));
  checkEq(c->nrefs, 2);

  rmref(origc);
}

void test_build_handles_literal_ellipses() {
  IndentSensitiveStream in("'...");
  Cell *c=nextCell(in);
  checkEq(c->nrefs, 0);
  checkEq(car(c), newSym("'"));
  checkEq(cdr(c), newSym("..."));
  checkEq(cdr(c)->nrefs, 2);
  rmref(c);
}

void test_build_handles_nested_form() {
  IndentSensitiveStream in("(3 7 (33 23))");
  Cell *c=nextCell(in), *origc=c;
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
}

void test_build_handles_strings() {
  IndentSensitiveStream in("(3 7 (33 \"abc\" 23))");
  Cell *c=nextCell(in), *origc=c;
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
    checkEq(toString(car(c2)), "abc");
    checkEq(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newNum(23));
    checkEq(car(c2)->nrefs, 2);
    checkEq(cdr(c2), nil);
  checkEq(cdr(c), nil);

  rmref(origc);
}

void test_build_handles_syms() {
  IndentSensitiveStream in("(3 7 (33 \"abc\" 3de 23))");
  Cell *c=nextCell(in), *origc=c;
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
    checkEq(toString(car(c2)), "abc");
    checkEq(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym("3de"));
    checkEq(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newNum(23));
    checkEq(car(c2)->nrefs, 2);
    checkEq(cdr(c2), nil);
  checkEq(cdr(c), nil);

  rmref(origc);
}

void test_build_handles_quotes() {
  IndentSensitiveStream in("`(34 ,(35) ,36 ,@37 @,38 @39 ,'(a))");
  Cell *c=nextCell(in), *origc=c;
  checkEq(c->nrefs, 0);
  checkEq(car(c), newSym("`"));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newNum(34));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
    Cell* c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym(","));
    c2 = cdr(c2);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newNum(35));
    checkEq(car(c2)->nrefs, 2);
    checkEq(cdr(c2), nil);
  c = cdr(c);
    c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym(","));
    checkEq(cdr(c2), newNum(36));
    checkEq(cdr(c2)->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym(",@"));
    checkEq(cdr(c2), newNum(37));
    checkEq(cdr(c2)->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym("@"));
    checkEq(car(cdr(c2)), newSym(","));
    checkEq(cdr(cdr(c2)), newNum(38));
    checkEq(cdr(cdr(c2))->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym("@"));
    checkEq(cdr(c2), newNum(39));
    checkEq(cdr(c2)->nrefs, 2);
  c = cdr(c);
    c2 = car(c);
    checkEq(c2->nrefs, 1);
    checkEq(car(c2), newSym(","));
    checkEq(cdr(c2)->nrefs, 1);
    c2 = cdr(c2);
    checkEq(car(c2), newSym("'"));
    c2 = cdr(c2);
    checkEq(car(c2), newSym("a"));
    checkEq(car(c2)->nrefs, 2);
    checkEq(cdr(c2), nil);
  checkEq(cdr(c), nil);

  rmref(origc);
}

void test_build_handles_indented_wrapped_lines() {
  IndentSensitiveStream in("a\n  (a b c\n   d e)");
  Cell *c0=nextCell(in);
  checkEq(c0, newSym("a"));
  checkEq(c0->nrefs, 1);

  Cell *c=nextCell(in), *origc=c;
  checkEq(c->nrefs, 0);
  checkEq(car(c), newSym("a"));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newSym("b"));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newSym("c"));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newSym("d"));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
  checkEq(c->nrefs, 1);
  checkEq(car(c), newSym("e"));
  checkEq(car(c)->nrefs, 2);
  c = cdr(c);
  checkEq(c, nil);
  rmref(origc);
  rmref(c0);
}
