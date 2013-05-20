void test_build_handles_nil() {
  stringstream in("()");
  readAll(in);
  checkTraceContents("cell", "nil\n");
}

void test_build_handles_nil2() {
  stringstream in("nil");
  readAll(in);
  checkTraceContents("cell", "nil\n");
}

void test_build_handles_integer() {
  stringstream in("34");
  readAll(in);
  checkTraceContents("cell", "num: 34\n");
}

void test_build_handles_float() {
  stringstream in("3.4");
  readAll(in);
  checkTraceContents("cell", "float: 3.4\n");
}

void test_build_warns_on_ambiguous_float() {
  stringstream in("-.4");
  readAll(in);
  CHECK_EQ(raiseCount, 1); raiseCount=0;
  checkTraceContents("cell", "float: -0.4\n");
}

void test_build_creates_floats_on_overflow() {
  stringstream in("100000000000000000000");
  readAll(in);
  CHECK_EQ(raiseCount, 1); raiseCount=0;
  checkTraceContents("cell", "float: 1e+20\n");
}

void test_build_handles_sym() {
  stringstream in("a");
  readAll(in);
  checkTraceContents("cell", "sym: a\n");
}

void test_build_handles_string() {
  stringstream in("\"a\"");
  readAll(in);
  checkTraceContents("cell", "string: \"a\"\n");
}

void test_build_doesnt_mix_syms_and_strings() {
  Cell* s = newString("a");
  CHECK(s != newSym("a"));
}

void test_build_handles_quoted_sym() {
  stringstream in("'a");
  list<Cell*> result = readAll(in);
  checkTraceContents2("cell", 1, "'a\n");
  checkTraceContents2("cell", 2, "sym: '\nsym: a\n");
  Cell* c = result.front();
  CHECK_EQ(car(c), newSym("'"));
  CHECK_EQ(cdr(c), newSym("a"));
}

void test_build_handles_multiple_atoms() {
  stringstream in("34\n35");
  readAll(in);
  checkTraceContents("cell", "num: 34\nnum: 35\n");
}

void test_build_handles_form() {
  stringstream in("(34 35)");
  list<Cell*> result = readAll(in);
  checkTraceContents2("cell", 1, "(34 35)\n");
  Cell* c = result.front();
  CHECK_EQ(car(c), newNum(34));
  c = cdr(c);
  CHECK_EQ(car(c), newNum(35));
  CHECK_EQ(cdr(c), nil);
}

void test_build_handles_dotted_list() {
  stringstream in("(34 ... 35)");
  list<Cell*> result = readAll(in);
  checkTraceContents2("cell", 1, "(34 ... 35)\n");
  Cell* c = result.front();
  CHECK_EQ(car(c), newNum(34));
  c = cdr(c);
  CHECK_EQ(c, newNum(35));
}

void test_build_handles_literal_ellipses() {
  stringstream in("'...");
  list<Cell*> result = readAll(in);
  checkTraceContents2("cell", 1, "'...\n");
  Cell *c = result.front();
  CHECK_EQ(car(c), newSym("'"));
  CHECK_EQ(cdr(c), newSym("..."));
}

void test_build_handles_nested_form() {
  stringstream in("(3 7 (33 23))");
  list<Cell*> result = readAll(in);
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
  stringstream in("(3 7 (33 \"abc\" 23))");
  list<Cell*> result = readAll(in);
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
  stringstream in("(3 7 (33 \"abc\" 3de 23))");
  list<Cell*> result = readAll(in);
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
  stringstream in("a\n  (a b c\n   d e)");
  list<Cell*> result = readAll(in);
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
