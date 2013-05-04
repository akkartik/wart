void test_hunks_works_without_metadata() {
  std::istringstream in("a\nb\nc\n");
  list<string> l = hunks(in).front().lines;
  checkEq(l.front(), "a");  l.pop_front();
  checkEq(l.front(), "b");  l.pop_front();
  checkEq(l.front(), "c");  l.pop_front();
  checkEq(l.front(), "");   l.pop_front();
  check(l.empty());
}

void test_hunks_works_with_metadata() {
  std::istringstream in("a\n:(a b)\nc\n");
  list<Hunk> h = hunks(in);
  list<string> l = h.front().lines;   h.pop_front();
    checkEq(l.front(), "a");  l.pop_front();
    checkEq(l.front(), "");   l.pop_front();
    checkEq(l.empty());
  l = h.front().lines;    h.pop_front();
    checkEq(l.front(), ":(a b)");  l.pop_front();
    checkEq(l.front(), "c");  l.pop_front();
    checkEq(l.front(), "");   l.pop_front();
    check(l.empty());
  checkEq(h.empty());
}
