void test_tokenize_handles_multiple_atoms() {
  read_all("34 abc 3.4");
  CHECK_TRACE_CONTENTS("tokenize", "34abc3.4");
}

void test_tokenize_handles_string_literal() {
  read_all("34 \"abc\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\"abc\"");
}

void test_tokenize_handles_multiple_lines() {
  read_all("34\n\"abc\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\"abc\"");
}

void test_tokenize_handles_string_with_space() {
  read_all("34\n\"abc def\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\"abc def\"");
}

void test_tokenize_handles_string_with_escape() {
  read_all("34\n\"abc \\\"quote def\"");
  CHECK_TRACE_CONTENTS("tokenize", "34\"abc \\\"quote def\"");
}

void test_tokenize_handles_comment() {
  read_all("()'a #abc def ghi");
  CHECK_TRACE_CONTENTS("tokenize", "()'a");
}
