void test_build_handles_nil() {
  read_all("()");
  CHECK_TRACE_CONTENTS("cell", "nil");
}

void test_build_handles_nil2() {
  read_all("nil");
  CHECK_TRACE_CONTENTS("cell", "nil");
}

void test_build_handles_integer() {
  read_all("34");
  CHECK_TRACE_CONTENTS("cell", "num: 34");
}

void test_build_handles_float() {
  read_all("3.4");
  CHECK_TRACE_CONTENTS("cell", "float: 3.4");
}

void test_build_warns_on_ambiguous_float() {
  read_all("-.4");
  CHECK_EQ(Raise_count, 1); Raise_count=0;
  CHECK_TRACE_CONTENTS("cell", "float: -0.4");
}

void test_build_creates_floats_on_overflow() {
  read_all("100000000000000000000");
  CHECK_EQ(Raise_count, 1); Raise_count=0;
  CHECK_TRACE_CONTENTS("cell", "float: 1e+20");
}

void test_build_handles_sym() {
  read_all("a");
  CHECK_TRACE_CONTENTS("cell", "sym: a");
}

void test_build_handles_string() {
  read_all("\"a\"");
  CHECK_TRACE_CONTENTS("cell", "string: \"a\"");
}

void test_build_doesnt_mix_syms_and_strings() {
  cell* s = new_string("a");
  CHECK(s != new_sym("a"));
}

void test_build_handles_quoted_sym() {
  list<cell*> result = read_all("'a");
  CHECK_TRACE_CONTENTS("cell", 1, "'a");
  CHECK_TRACE_CONTENTS("cell", 2, "sym: 'sym: a");
  cell* c = result.front();
  CHECK_EQ(car(c), new_sym("'"));
  CHECK_EQ(cdr(c), new_sym("a"));
}

void test_build_handles_multiple_atoms() {
  list<cell*> result = read_all("34\n35");
  CHECK_TRACE_CONTENTS("cell", "num: 34num: 35");
}

void test_build_handles_form() {
  list<cell*> result = read_all("(34 35)");
  CHECK_TRACE_TOP("cell", "(34 35)");
  cell* c = result.front();
  CHECK_EQ(car(c), new_num(34));
  c = cdr(c);
  CHECK_EQ(car(c), new_num(35));
  CHECK_EQ(cdr(c), nil);
}

void test_build_handles_dotted_list() {
  list<cell*> result = read_all("(34 ... 35)");
  CHECK_TRACE_TOP("cell", "(34 ... 35)");
  cell* c = result.front();
  CHECK_EQ(car(c), new_num(34));
  c = cdr(c);
  CHECK_EQ(c, new_num(35));
}

void test_build_handles_literal_ellipses() {
  list<cell*> result = read_all("'...");
  CHECK_TRACE_TOP("cell", "'...");
  cell *c = result.front();
  CHECK_EQ(car(c), new_sym("'"));
  CHECK_EQ(cdr(c), new_sym("..."));
}

void test_build_handles_nested_form() {
  list<cell*> result = read_all("(3 7 (33 23))");
  CHECK_TRACE_TOP("cell", "(3 7 (33 23))");
  cell* c = result.front();
  CHECK_EQ(car(c), new_num(3));
  c = cdr(c);
  CHECK_EQ(car(c), new_num(7));
  c = cdr(c);
    cell* c2 = car(c);
    CHECK_EQ(car(c2), new_num(33));
    c2 = cdr(c2);
    CHECK_EQ(car(c2), new_num(23));
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);
}

void test_build_handles_strings() {
  list<cell*> result = read_all("(3 7 (33 \"abc\" 23))");
  CHECK_TRACE_TOP("cell", "(3 7 (33 \"abc\" 23))");
  cell* c = result.front();
  CHECK_EQ(car(c), new_num(3));
  c = cdr(c);
  CHECK_EQ(car(c), new_num(7));
  c = cdr(c);
    cell* c2 = car(c);
    CHECK_EQ(car(c2), new_num(33));
    c2 = cdr(c2);
    CHECK(is_string(car(c2)));
    CHECK_EQ(to_string(car(c2)), "abc");
    c2 = cdr(c2);
    CHECK_EQ(car(c2), new_num(23));
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);
}

void test_build_handles_syms() {
  list<cell*> result = read_all("(3 7 (33 \"abc\" 3de 23))");
  CHECK_TRACE_TOP("cell", "(3 7 (33 \"abc\" 3de 23))");
  cell* c = result.front();
  CHECK_EQ(car(c), new_num(3));
  c = cdr(c);
  CHECK_EQ(car(c), new_num(7));
  c = cdr(c);
    cell* c2 = car(c);
    CHECK_EQ(car(c2), new_num(33));
    c2 = cdr(c2);
    CHECK(is_string(car(c2)));
    CHECK_EQ(to_string(car(c2)), "abc");
    c2 = cdr(c2);
    CHECK_EQ(car(c2), new_sym("3de"));
    c2 = cdr(c2);
    CHECK_EQ(car(c2), new_num(23));
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);
}

void test_build_handles_indented_wrapped_lines() {
  list<cell*> result = read_all("a\n  (a b c\n   d e)");
  CHECK_TRACE_TOP("cell", "sym: a(a b c d e)");
  cell* c0 = result.front();  result.pop_front();
  CHECK_EQ(c0, new_sym("a"));

  cell* c = result.front();
  CHECK_EQ(car(c), new_sym("a"));
  c = cdr(c);
  CHECK_EQ(car(c), new_sym("b"));
  c = cdr(c);
  CHECK_EQ(car(c), new_sym("c"));
  c = cdr(c);
  CHECK_EQ(car(c), new_sym("d"));
  c = cdr(c);
  CHECK_EQ(car(c), new_sym("e"));
  c = cdr(c);
  CHECK_EQ(c, nil);
}
