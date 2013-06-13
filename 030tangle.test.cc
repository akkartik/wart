//? void test_hunks_works_without_metadata() {
//?   std::istringstream in("a\nb\nc\n");
//?   hunks(in);
//?   CHECK_TRACE_CONTENTS("tangle", "line: aline: bline: c");
//? }
//? 
//? void test_hunks_works_with_metadata() {
//?   std::istringstream in("a\n:(a b)\nc\n");
//?   hunks(in);
//?   CHECK_TRACE_CONTENTS("tangle", "a\n:(a b) =>\nc\n");
//? }
//? 
//? void test_hunks_works_with_indented_metadata() {
//?   std::istringstream in("a\n  :(a b)\n  c\n    d\n");
//?   hunks(in);
//?   CHECK_TRACE_CONTENTS("tangle", "a:(a b) =>\nc\n  d");
//? }

void test_foo() {
//?   ifstream in("x");
  std::istringstream in("a\nb\n:(a b)\n");
  list<string> out;
  process_next_hunk(in, "", out);
  exit(0);
}



void test_trim() {
  CHECK_EQ(trim(""), "");
  CHECK_EQ(trim(" "), "");
  CHECK_EQ(trim("  "), "");
  CHECK_EQ(trim("a"), "a");
  CHECK_EQ(trim(" a"), "a");
  CHECK_EQ(trim("  a"), "a");
  CHECK_EQ(trim("  ab"), "ab");
  CHECK_EQ(trim("a "), "a");
  CHECK_EQ(trim("a  "), "a");
  CHECK_EQ(trim("ab  "), "ab");
  CHECK_EQ(trim(" a "), "a");
  CHECK_EQ(trim("  a  "), "a");
  CHECK_EQ(trim("  ab  "), "ab");
}

void test_strip_indent() {
  CHECK_EQ(strip_indent("", 0), "");
  CHECK_EQ(strip_indent("", 1), "");
  CHECK_EQ(strip_indent("", 3), "");
  CHECK_EQ(strip_indent(" ", 0), " ");
  CHECK_EQ(strip_indent(" a", 0), " a");
  CHECK_EQ(strip_indent(" ", 1), "");
  CHECK_EQ(strip_indent(" a", 1), "a");
  CHECK_EQ(strip_indent(" ", 2), "");
  CHECK_EQ(strip_indent(" a", 2), "a");
  CHECK_EQ(strip_indent("  ", 0), "  ");
  CHECK_EQ(strip_indent("  a", 0), "  a");
  CHECK_EQ(strip_indent("  ", 1), " ");
  CHECK_EQ(strip_indent("  a", 1), " a");
  CHECK_EQ(strip_indent("  ", 2), "");
  CHECK_EQ(strip_indent("  a", 2), "a");
  CHECK_EQ(strip_indent("  ", 3), "");
  CHECK_EQ(strip_indent("  a", 3), "a");
}
