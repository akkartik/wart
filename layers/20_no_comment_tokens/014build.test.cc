void test_build_handles_nil() {
  stringstream in("()");
  checkEq(nextCell(in), nil);
}

void test_build_handles_nil2() {
  stringstream in("nil");
  checkEq(nextCell(in), nil);
}

void test_build_handles_integer() {
  stringstream in("34");
  Cell* c = nextCell(in);
  checkEq(c, newNum(34));
}

void test_build_handles_float() {
  stringstream in("3.4");
  Cell* c = nextCell(in);
  check(isNum(c));
  check(equalFloats(toFloat(c), 3.4));
}

void test_build_warns_on_ambiguous_float() {
  stringstream in("-.4");
  Cell* c = nextCell(in);
  checkEq(raiseCount, 1); raiseCount=0;
  check(isNum(c));
  check(equalFloats(toFloat(c), -0.4));
}

void test_build_creates_floats_on_overflow() {
  stringstream in("100000000000000000000");
  Cell* c = nextCell(in);
  checkEq(raiseCount, 1); raiseCount=0;   // overflow warning
  checkEq(c->type, FLOAT);
}

void test_build_handles_sym() {
  stringstream in("a");
  Cell* c = nextCell(in);
  checkEq(c, newSym("a"));
}

void test_build_handles_string() {
  stringstream in("\"a\"");
  Cell* c = nextCell(in);
  checkEq(toString(c), "a");
}

void test_build_doesnt_mix_syms_and_strings() {
  Cell* s = newString("a");
  check(s != newSym("a"));
}

void test_build_handles_quoted_sym() {
  stringstream in("'a");
  Cell* c = nextCell(in);
  checkEq(car(c), newSym("'"));
  checkEq(cdr(c), newSym("a"));
}

void test_build_handles_nested_quote() {
  stringstream in("',a");
  Cell* c = nextCell(in);
  checkEq(car(c), newSym("'"));
  checkEq(car(cdr(c)), newSym(","));
  checkEq(cdr(cdr(c)), newSym("a"));
}

void test_build_handles_multiple_atoms() {
  stringstream in("34\n35");
  Cell* c = nextCell(in);
  checkEq(c, newNum(34));
  checkEq(cdr(c), nil);

  c = nextCell(in);
  checkEq(c, newNum(35));
  checkEq(cdr(c), nil);
}

void test_build_handles_form() {
  stringstream in("(34 35)");
  Cell* c=nextCell(in);
  checkEq(car(c), newNum(34));

  c = cdr(c);
  checkEq(car(c), newNum(35));

  checkEq(cdr(c), nil);
}

void test_build_handles_dotted_list() {
  stringstream in("(34 ... 35)");
  Cell* c=nextCell(in);
  checkEq(car(c), newNum(34));

  c = cdr(c);
  checkEq(c, newNum(35));
}

void test_build_handles_literal_ellipses() {
  stringstream in("'...");
  Cell *c=nextCell(in);
  checkEq(car(c), newSym("'"));
  checkEq(cdr(c), newSym("..."));
}

void test_build_handles_nested_form() {
  stringstream in("(3 7 (33 23))");
  Cell* c=nextCell(in);
  checkEq(car(c), newNum(3));

  c = cdr(c);
  checkEq(car(c), newNum(7));

  c = cdr(c);
    Cell* c2 = car(c);
    checkEq(car(c2), newNum(33));
    c2 = cdr(c2);
    checkEq(car(c2), newNum(23));
    checkEq(cdr(c2), nil);
  checkEq(cdr(c), nil);
}

void test_build_handles_strings() {
  stringstream in("(3 7 (33 \"abc\" 23))");
  Cell* c=nextCell(in);
  checkEq(car(c), newNum(3));
  c = cdr(c);
  checkEq(car(c), newNum(7));
  c = cdr(c);
    Cell* c2 = car(c);
    checkEq(car(c2), newNum(33));
    c2 = cdr(c2);
    check(isString(car(c2)));
    checkEq(toString(car(c2)), "abc");
    c2 = cdr(c2);
    checkEq(car(c2), newNum(23));
    checkEq(cdr(c2), nil);
  checkEq(cdr(c), nil);
}

void test_build_handles_syms() {
  stringstream in("(3 7 (33 \"abc\" 3de 23))");
  Cell* c=nextCell(in);
  checkEq(car(c), newNum(3));
  c = cdr(c);
  checkEq(car(c), newNum(7));
  c = cdr(c);
    Cell* c2 = car(c);
    checkEq(car(c2), newNum(33));
    c2 = cdr(c2);
    check(isString(car(c2)));
    checkEq(toString(car(c2)), "abc");
    c2 = cdr(c2);
    checkEq(car(c2), newSym("3de"));
    c2 = cdr(c2);
    checkEq(car(c2), newNum(23));
    checkEq(cdr(c2), nil);
  checkEq(cdr(c), nil);
}

void test_build_handles_indented_wrapped_lines() {
  stringstream in("a\n  (a b c\n   d e)");
  Cell *c0=nextCell(in);
  checkEq(c0, newSym("a"));

  Cell* c=nextCell(in);
  checkEq(car(c), newSym("a"));
  c = cdr(c);
  checkEq(car(c), newSym("b"));
  c = cdr(c);
  checkEq(car(c), newSym("c"));
  c = cdr(c);
  checkEq(car(c), newSym("d"));
  c = cdr(c);
  checkEq(car(c), newSym("e"));
  c = cdr(c);
  checkEq(c, nil);
}
