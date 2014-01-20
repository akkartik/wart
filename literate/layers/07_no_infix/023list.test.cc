void test_list_range() {
  run("(list_range '(3 4 5) 0 2)");
  CHECK_TRACE_TOP("eval", "=> (3 4)");
}

void test_list_splice_replaces_index() {
  run("(<- a '(3 4))");
  run("(list_splice a 1 2 '(5))");
  CLEAR_TRACE;
  run("a");
  CHECK_TRACE_TOP("eval", "=> (3 5)");
  end_dynamic_scope("a");
}

void test_list_splice_replaces_first_index() {
  run("(<- a '(3 4))");
  run("(list_splice a 0 1 '(9))");
  CLEAR_TRACE;
  run("a");
  CHECK_TRACE_TOP("eval", "=> (9 4)");
  end_dynamic_scope("a");
}

void test_list_splice_inserts_list_at_beginning() {
  run("(<- a '(3 4))");
  run("(list_splice a 0 1 '(7 9))");
  CLEAR_TRACE;
  run("a");
  CHECK_TRACE_TOP("eval", "=> (7 9 4)");
  end_dynamic_scope("a");
}

void test_list_splice_inserts_shorter_list() {
  run("(<- a '(3 4 5))");
  run("(list_splice a 1 3 '(6))");
  CLEAR_TRACE;
  run("a");
  CHECK_TRACE_TOP("eval", "=> (3 6)");
  end_dynamic_scope("a");
}

void test_list_splice_inserts_longer_list() {
  run("(<- a '(3 4 5))");
  run("(list_splice a 1 2 '(6 7 8))");
  CLEAR_TRACE;
  run("a");
  CHECK_TRACE_TOP("eval", "=> (3 6 7 8 5)");
  end_dynamic_scope("a");
}

void test_list_splice_inserts_nil() {
  run("(<- a '(3 4))");
  run("(list_splice a 0 1 '(nil))");
  CLEAR_TRACE;
  run("a");
  CHECK_TRACE_TOP("eval", "=> (nil 4)");
  end_dynamic_scope("a");
}

void test_list_splice_deletes() {
  run("(<- a '(3 4))");
  run("(list_splice a 1 2 nil)");
  CLEAR_TRACE;
  run("a");
  CHECK_TRACE_TOP("eval", "=> (3)");
  end_dynamic_scope("a");
}

void test_list_splice_deletes_at_start() {
  run("(<- a '(3 4))");
  run("(list_splice a 0 1 nil)");
  CLEAR_TRACE;
  run("a");
  CHECK_TRACE_TOP("eval", "=> (4)");
  end_dynamic_scope("a");
}

void test_list_splice_deletes_entire() {
  run("(<- a '(3 4))");
  run("(list_splice a 0 2 nil)");
  CLEAR_TRACE;
  run("a");
  CHECK_TRACE_TOP("eval", "=> nil");
  end_dynamic_scope("a");
}

void test_list_splice_returns_list_being_spliced_in() {
  run("(<- a '(3 4 5))");
  run("(list_splice a 1 2 '(6 7 8))");
  CHECK_TRACE_TOP("eval", "=> (6 7 8)");
  end_dynamic_scope("a");
}

void test_nth_cdr() {
  TEMP(x, mkref(new_cons(new_num(3), new_cons(new_num(4)))));
  CHECK_EQ(nth_cdr(x, 0), x);
  CHECK_EQ(car(nth_cdr(x, 1)), new_num(4));
  CHECK_EQ(nth_cdr(x, 2), nil);
}

void test_contains_handles_circular_lists() {
  unordered_set<cell*> done;
  TEMP(x, mkref(new_cons(new_num(1))));
  set_cdr(x, x);
  CHECK(!contains(x, new_sym("a"), done));
  x->cdr = nil;  --x->nrefs;  // manually break cycle for gc
}

void test_list_sort() {
  run("(sort (fn(a b) (< (len a) (len b))) '(\"abc\" \"d\" \"ef\"))");
  CHECK_TRACE_TOP("eval", "=> (\"d\" \"ef\" \"abc\")");
}
