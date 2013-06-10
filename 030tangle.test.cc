void test_hunks_works_without_metadata() {
  std::istringstream in("a\nb\nc\n");
  hunks(in);
  CHECK_TRACE_CONTENTS("tangle", "hunk: ahunk: bhunk: c");
}



void test_trim() {
  string s = "  abc  ";
  trim(s);
  CHECK_EQ(s, "abc");

  s = "  abc";
  trim(s);
  CHECK_EQ(s, "abc");

  s = " a";
  trim(s);
  CHECK_EQ(s, "a");

  s = " a ";
  trim(s);
  CHECK_EQ(s, "a");

  s = "  ";
  trim(s);
  CHECK_EQ(s, "");
}
