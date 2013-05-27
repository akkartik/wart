void test_build_handles_nil() {
  readAll("()");
  checkTraceContents("cell", "nil\n");
}

void test_build_handles_nil2() {
  readAll("nil");
  checkTraceContents("cell", "nil\n");
}

void test_build_handles_integer() {
  readAll("34");
  checkTraceContents("cell", "num: 34\n");
}

void test_build_handles_float() {
  readAll("3.4");
  checkTraceContents("cell", "float: 3.4\n");
}

void test_build_warns_on_ambiguous_float() {
  readAll("-.4");
  CHECK_EQ(raiseCount, 1); raiseCount=0;
  checkTraceContents("cell", "float: -0.4\n");
}

void test_build_creates_floats_on_overflow() {
  readAll("100000000000000000000");
  CHECK_EQ(raiseCount, 1); raiseCount=0;
  checkTraceContents("cell", "float: 1e+20\n");
}

void test_build_handles_sym() {
  readAll("a");
  checkTraceContents("cell", "sym: a\n");
}

void test_build_handles_string() {
  readAll("\"a\"");
  checkTraceContents("cell", "string: \"a\"\n");
}

void test_build_doesnt_mix_syms_and_strings() {
  Cell* s = newString("a");
  CHECK(s != newSym("a"));
}

void test_build_handles_quoted_sym() {
  list<Cell*> result = readAll("'a");
  checkTraceContents2("cell", 1, "'a\n");
  checkTraceContents2("cell", 2, "sym: '\nsym: a\n");
  Cell* c = result.front();
  CHECK_EQ(car(c), newSym("'"));
  CHECK_EQ(cdr(c), newSym("a"));
}

void test_build_handles_multiple_atoms() {
  list<Cell*> result = readAll("34\n35");
  checkTraceContents("cell", "num: 34\nnum: 35\n");
}

void test_build_handles_form() {
  list<Cell*> result = readAll("(34 35)");
  checkTraceContents2("cell", 1, "(34 35)\n");
  Cell* c = result.front();
  CHECK_EQ(car(c), newNum(34));
  c = cdr(c);
  CHECK_EQ(car(c), newNum(35));
  CHECK_EQ(cdr(c), nil);
}

void test_build_handles_dotted_list() {
  list<Cell*> result = readAll("(34 ... 35)");
  checkTraceContents2("cell", 1, "(34 ... 35)\n");
  Cell* c = result.front();
  CHECK_EQ(car(c), newNum(34));
  c = cdr(c);
  CHECK_EQ(c, newNum(35));
}

void test_build_handles_literal_ellipses() {
  list<Cell*> result = readAll("'...");
  checkTraceContents2("cell", 1, "'...\n");
  Cell *c = result.front();
  CHECK_EQ(car(c), newSym("'"));
  CHECK_EQ(cdr(c), newSym("..."));
}

void test_build_handles_nested_form() {
  list<Cell*> result = readAll("(3 7 (33 23))");
  checkTraceContents2("cell", 1, "(3 7 (33 23))\n");
  Cell* c = result.front();
  CHECK_EQ(car(c), newNum(3));
  c = cdr(c);
  CHECK_EQ(car(c), newNum(7));
  c = cdr(c);
    Cell* c2 = car(c);
    CHECK_EQ(car(c2), newNum(33));
    c2 = cdr(c2);
    CHECK_EQ(car(c2), newNum(23));
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);
}

void test_build_handles_strings() {
  list<Cell*> result = readAll("(3 7 (33 \"abc\" 23))");
  checkTraceContents2("cell", 1, "(3 7 (33 \"abc\" 23))\n");
  Cell* c = result.front();
  CHECK_EQ(car(c), newNum(3));
  c = cdr(c);
  CHECK_EQ(car(c), newNum(7));
  c = cdr(c);
    Cell* c2 = car(c);
    CHECK_EQ(car(c2), newNum(33));
    c2 = cdr(c2);
    CHECK(isString(car(c2)));
    CHECK_EQ(toString(car(c2)), "abc");
    c2 = cdr(c2);
    CHECK_EQ(car(c2), newNum(23));
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);
}

void test_build_handles_syms() {
  list<Cell*> result = readAll("(3 7 (33 \"abc\" 3de 23))");
  checkTraceContents2("cell", 1, "(3 7 (33 \"abc\" 3de 23))\n");
  Cell* c = result.front();
  CHECK_EQ(car(c), newNum(3));
  c = cdr(c);
  CHECK_EQ(car(c), newNum(7));
  c = cdr(c);
    Cell* c2 = car(c);
    CHECK_EQ(car(c2), newNum(33));
    c2 = cdr(c2);
    CHECK(isString(car(c2)));
    CHECK_EQ(toString(car(c2)), "abc");
    c2 = cdr(c2);
    CHECK_EQ(car(c2), newSym("3de"));
    c2 = cdr(c2);
    CHECK_EQ(car(c2), newNum(23));
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);
}

void test_build_handles_indented_wrapped_lines() {
  list<Cell*> result = readAll("a\n  (a b c\n   d e)");
  checkTraceContents2("cell", 1, "sym: a\n(a b c d e)\n");
  Cell* c0 = result.front();  result.pop_front();
  CHECK_EQ(c0, newSym("a"));

  Cell* c = result.front();
  CHECK_EQ(car(c), newSym("a"));
  c = cdr(c);
  CHECK_EQ(car(c), newSym("b"));
  c = cdr(c);
  CHECK_EQ(car(c), newSym("c"));
  c = cdr(c);
  CHECK_EQ(car(c), newSym("d"));
  c = cdr(c);
  CHECK_EQ(car(c), newSym("e"));
  c = cdr(c);
  CHECK_EQ(c, nil);
}
