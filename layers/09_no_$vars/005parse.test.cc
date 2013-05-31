void test_parse_handles_empty_stream() {
  read_all("");
  CHECK_TRACE_CONTENTS("parse", "");
}

void test_parse_handles_trailing_comment() {
  read_all("34 # abc");
  CHECK_TRACE_CONTENTS("parse", "34");
}

void test_parse_handles_atom() {
  read_all("34");
  CHECK_TRACE_CONTENTS("parse", "34");
}

void test_parse_handles_atoms() {
  read_all("34\n\"a b c\"\n3.4");
  CHECK_TRACE_CONTENTS("parse", "34\"a b c\"3.4");
}

void test_parse_handles_forms() {
  read_all("(34 \"a b c\")");
  CHECK_TRACE_TOP("parse", "(34 \"a b c\")");
}

void test_parse_handles_nested_forms() {
  read_all("(34 (2 3) \"a b c\")");
  CHECK_TRACE_CONTENTS("parse", 1, "(34 (2 3) \"a b c\")");
  CHECK_TRACE_CONTENTS("parse", 2, "34(2 3)\"a b c\")");
  CHECK_TRACE_CONTENTS("parse", 3, "23)");
}

void test_parse_handles_nested_forms_with_comments() {
  read_all("(a b (c d #\n))");
  CHECK_TRACE_CONTENTS("parse", 1, "(a b (c d))");
  CHECK_TRACE_CONTENTS("parse", 2, "ab(c d))");
}

void test_parse_handles_quotes() {
  read_all("(34 `(2 ,b) ',35 ,',36 ,'a)");
  CHECK_TRACE_CONTENTS("parse", 1, "(34 `(2 ,b) ',35 ,',36 ,'a)");
  CHECK_TRACE_CONTENTS("parse", 2, "34`(2 ,b)',35,',36,'a");
}

void test_parse_handles_splice_operators() {
  read_all("`(2 ,@b)");
  CHECK_TRACE_CONTENTS("parse", 1, "`(2 ,@b)");
  CHECK_TRACE_CONTENTS("parse", 2, "2,@b)");
}
