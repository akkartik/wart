cell* read(string s) {
  return read(*new stringstream(s));
}

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
  Do_raise = false;
  read_all("-.4");
  CHECK_EQ(Raise_count, 1); Raise_count=0;
  CHECK_TRACE_CONTENTS("cell", "float: -0.4");
}

void test_build_creates_floats_on_overflow() {
  Do_raise = false;
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
  TEMP(s, new_string("a"));
  CHECK(s != new_sym("a"));
}

void test_build_handles_quoted_sym() {
  TEMP(c, read("'a"));
  CHECK_TRACE_CONTENTS("cell", 1, "'a");
  CHECK_TRACE_CONTENTS("cell", 2, "sym: 'sym: a");
  CHECK_EQ(car(c), new_sym("'"));
  CHECK_EQ(cdr(c), new_sym("a"));
}

void test_build_handles_nested_quote() {
  read_all("',a");
  CHECK_TRACE_CONTENTS("cell", 1, "',a");
  CHECK_TRACE_CONTENTS("cell", 2, ",a");
  CHECK_TRACE_CONTENTS("cell", 3, "sym: a");
  CHECK_TRACE_CONTENTS("cell", /*any frame*/ "sym: 'sym: ,sym: a");
}

void test_build_handles_multiple_atoms() {
  read_all("34\n35");
  CHECK_TRACE_CONTENTS("cell", "num: 34num: 35");
}

void test_build_handles_form() {
  read_all("(34 35)");
  CHECK_TRACE_CONTENTS("cell", 2, "(34 35)");
  CHECK_TRACE_CONTENTS("cell", 3, "(35)");
  CHECK_TRACE_CONTENTS("cell", 4, "nil");
  CHECK_TRACE_CONTENTS("cell", /*any frame*/ "num: 34num: 35");
}

void test_build_handles_dotted_list() {
  read_all("(34 ... 35)");
  CHECK_TRACE_CONTENTS("cell", 2, "(34 ... 35)");
  CHECK_TRACE_CONTENTS("cell", 3, "num: 35");
  CHECK_TRACE_CONTENTS("cell", /*any frame*/ "num: 34num: 35");
}

void test_build_handles_literal_ellipses() {
  TEMP(c, read("'..."));
  CHECK_TRACE_TOP("cell", "'...");
  CHECK_EQ(car(c), new_sym("'"));
  CHECK_EQ(cdr(c), new_sym("..."));
}

void test_build_handles_nested_form() {
  read_all("(3 7 (33 23))");
  CHECK_TRACE_CONTENTS("cell", 2, "(3 7 (33 23))");
  CHECK_TRACE_CONTENTS("cell", 3, "(7 (33 23))");
  CHECK_TRACE_CONTENTS("cell", 4, "((33 23))");
  CHECK_TRACE_CONTENTS("cell", 5, "nil");
  CHECK_TRACE_CONTENTS("cell", /*any frame*/ "num: 3num: 7num: 33num: 23(33 23)");
}

void test_build_handles_strings() {
  read_all("(3 7 (33 \"abc\" 23))");
  CHECK_TRACE_CONTENTS("cell", 2, "(3 7 (33 \"abc\" 23))");
  CHECK_TRACE_CONTENTS("cell", /*any frame*/ "string: \"abc\"");
}

void test_build_handles_syms() {
  read_all("(3 7 (33 \"abc\" 3de 23))");
  CHECK_TRACE_TOP("cell", "(3 7 (33 \"abc\" 3de 23))");
  CHECK_TRACE_CONTENTS("cell", 2, "(3 7 (33 \"abc\" 3de 23))");
  CHECK_TRACE_CONTENTS("cell", /*any frame*/ "sym: 3de");
}

void test_build_handles_quotes() {
  read_all("`(34 ,(35) ,36 ,@37 ,'(a))");
  CHECK_TRACE_CONTENTS("cell", 1, "`(34 ,(35) ,36 ,@37 ,'(a))");
  CHECK_TRACE_CONTENTS("cell", 2, "(34 ,(35) ,36 ,@37 ,'(a))");
  CHECK_TRACE_CONTENTS("cell", 4, "(,(35) ,36 ,@37 ,'(a))");
  CHECK_TRACE_CONTENTS("cell", 5, "(,36 ,@37 ,'(a))");
  CHECK_TRACE_CONTENTS("cell", 6, "(,@37 ,'(a))");
  CHECK_TRACE_CONTENTS("cell", 7, "(,'(a))");
  CHECK_TRACE_CONTENTS("cell", 8, "nil");
  CHECK_TRACE_CONTENTS("cell", /*any frame*/ "sym: `num: 34sym: ,num: 35sym: ,num: 36sym: ,@num: 37sym: ,sym: 'sym: a");
}

void test_build_handles_indented_wrapped_lines() {
  read_all("a\n  (a b c\n   d e)");
  CHECK_TRACE_TOP("cell", "sym: a(a b c d e)");
  CHECK_TRACE_CONTENTS("cell", 2, "(a b c d e)");
  CHECK_TRACE_CONTENTS("cell", 3, "(b c d e)");
  CHECK_TRACE_CONTENTS("cell", 4, "(c d e)");
  CHECK_TRACE_CONTENTS("cell", 5, "(d e)");
  CHECK_TRACE_CONTENTS("cell", 6, "(e)");
  CHECK_TRACE_CONTENTS("cell", 7, "nil");
}
