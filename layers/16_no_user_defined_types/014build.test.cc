// CHECK all nrefs except quotes

void test_build_handles_nil() {
  stringstream in("()");
  CHECK_EQ(next_cell(in), nil);
}

void test_build_handles_nil2() {
  stringstream in("nil");
  CHECK_EQ(next_cell(in), nil);
}

void test_build_handles_integer() {
  stringstream in("34");
  cell* c = next_cell(in);
  CHECK_EQ(c, new_num(34));
  CHECK_EQ(c->nrefs, 1);
  rmref(c);
}

void test_build_handles_float() {
  stringstream in("3.4");
  cell* c = next_cell(in);
  CHECK(is_num(c));
  CHECK(equal_floats(to_float(c), 3.4));
  CHECK_EQ(c->nrefs, 0);   // floats aren't interned
  rmref(c);
}

void test_build_warns_on_ambiguous_float() {
  stringstream in("-.4");
  cell* c = next_cell(in);
  CHECK_EQ(Raise_count, 1); Raise_count=0;
  CHECK(is_num(c));
  CHECK(equal_floats(to_float(c), -0.4));
  rmref(c);
}

void test_build_creates_floats_on_overflow() {
  stringstream in("100000000000000000000");
  cell* c = next_cell(in);
  CHECK_EQ(Raise_count, 1); Raise_count=0;   // overflow warning
  CHECK_EQ(c->type, FLOAT);
  CHECK_EQ(c->nrefs, 0);
  rmref(c);
}

void test_build_handles_sym() {
  stringstream in("a");
  cell* c = next_cell(in);
  CHECK_EQ(c, new_sym("a"));
  CHECK_EQ(c->nrefs, 1);
  rmref(c);
}

void test_build_handles_string() {
  stringstream in("\"a\"");
  cell* c = next_cell(in);
  CHECK_EQ(to_string(c), "a");
  CHECK_EQ(c->nrefs, 0);   // strings aren't interned
  rmref(c);
}

void test_build_doesnt_mix_syms_and_strings() {
  cell* s = new_string("a");
  CHECK(s != new_sym("a"));
  rmref(s);
}

void test_build_handles_quoted_sym() {
  stringstream in("'a");
  cell* c = next_cell(in);
  CHECK_EQ(car(c), new_sym("'"));
  CHECK_EQ(cdr(c), new_sym("a"));
  CHECK_EQ(cdr(c)->nrefs, 2);
  rmref(c);
}

void test_build_handles_multiple_atoms() {
  stringstream in("34\n35");
  cell* c = next_cell(in);
  CHECK_EQ(c, new_num(34));
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(cdr(c), nil);

  c = next_cell(in);
  CHECK_EQ(c, new_num(35));
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(cdr(c), nil);
}

void test_build_handles_form() {
  stringstream in("(34 35)");
  cell *c=next_cell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), new_num(34));
  CHECK_EQ(car(c)->nrefs, 2);

  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), new_num(35));
  CHECK_EQ(car(c)->nrefs, 2);

  CHECK_EQ(cdr(c), nil);
  rmref(origc);
}

void test_build_handles_dotted_list() {
  stringstream in("(34 ... 35)");
  cell *c=next_cell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), new_num(34));
  CHECK_EQ(car(c)->nrefs, 2);

  c = cdr(c);
  CHECK_EQ(c, new_num(35));
  CHECK_EQ(c->nrefs, 2);

  rmref(origc);
}

void test_build_handles_literal_ellipses() {
  stringstream in("'...");
  cell *c=next_cell(in);
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), new_sym("'"));
  CHECK_EQ(cdr(c), new_sym("..."));
  CHECK_EQ(cdr(c)->nrefs, 2);
  rmref(c);
}

void test_build_handles_nested_form() {
  stringstream in("(3 7 (33 23))");
  cell *c=next_cell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), new_num(3));
  CHECK_EQ(car(c)->nrefs, 2);

  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), new_num(7));
  CHECK_EQ(car(c)->nrefs, 2);

  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
    cell* c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), new_num(33));
    CHECK_EQ(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), new_num(23));
    CHECK_EQ(car(c2)->nrefs, 2);
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);

  rmref(origc);
}

void test_build_handles_strings() {
  stringstream in("(3 7 (33 \"abc\" 23))");
  cell *c=next_cell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), new_num(3));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), new_num(7));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
    cell* c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), new_num(33));
    CHECK_EQ(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK(is_string(car(c2)));
    CHECK_EQ(to_string(car(c2)), "abc");
    CHECK_EQ(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), new_num(23));
    CHECK_EQ(car(c2)->nrefs, 2);
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);

  rmref(origc);
}

void test_build_handles_syms() {
  stringstream in("(3 7 (33 \"abc\" 3de 23))");
  cell *c=next_cell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), new_num(3));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), new_num(7));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
    cell* c2 = car(c);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), new_num(33));
    CHECK_EQ(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK(is_string(car(c2)));
    CHECK_EQ(to_string(car(c2)), "abc");
    CHECK_EQ(car(c2)->nrefs, 1);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), new_sym("3de"));
    CHECK_EQ(car(c2)->nrefs, 2);
    c2 = cdr(c2);
    CHECK_EQ(c2->nrefs, 1);
    CHECK_EQ(car(c2), new_num(23));
    CHECK_EQ(car(c2)->nrefs, 2);
    CHECK_EQ(cdr(c2), nil);
  CHECK_EQ(cdr(c), nil);

  rmref(origc);
}

void test_build_handles_indented_wrapped_lines() {
  stringstream in("a\n  (a b c\n   d e)");
  cell *c0=next_cell(in);
  CHECK_EQ(c0, new_sym("a"));
  CHECK_EQ(c0->nrefs, 1);

  cell *c=next_cell(in), *origc=c;
  CHECK_EQ(c->nrefs, 0);
  CHECK_EQ(car(c), new_sym("a"));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), new_sym("b"));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), new_sym("c"));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), new_sym("d"));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c->nrefs, 1);
  CHECK_EQ(car(c), new_sym("e"));
  CHECK_EQ(car(c)->nrefs, 2);
  c = cdr(c);
  CHECK_EQ(c, nil);
  rmref(origc);
  rmref(c0);
}
