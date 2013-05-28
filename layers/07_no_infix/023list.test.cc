void test_list_range() {
  cell* expr = read("(list_range '(3 4 5) 0 2)");
  cell* result = eval(expr);
  // (3 4)
  CHECK_EQ(car(result), new_num(3));
  CHECK_EQ(car(cdr(result)), new_num(4));
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  rmref(expr);
}

void test_list_splice_replaces_index() {
  new_dynamic_scope("a", new_cons(new_num(3), new_cons(new_num(4))));   // (3 4)
  cell* expr = read("(list_splice a 1 2 '(5))");
  cell* call = eval(expr);
  // (3 5)
  CHECK_EQ(car(lookup("a")), new_num(3));
  CHECK_EQ(car(cdr(lookup("a"))), new_num(5));
  CHECK_EQ(cdr(cdr(lookup("a"))), nil);
  rmref(call);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_list_splice_replaces_first_index() {
  new_dynamic_scope("a", new_cons(new_num(3), new_cons(new_num(4))));   // (3 4)
  cell* expr = read("(list_splice a 0 1 '(9))");
  cell* call = eval(expr);
  // (9 4)
  CHECK_EQ(car(lookup("a")), new_num(9));
  rmref(call);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_list_splice_inserts_list_at_beginning() {
  new_dynamic_scope("a", new_cons(new_num(3), new_cons(new_num(4))));   // (3 4)
  cell* expr = read("(list_splice a 0 1 '(7 9))");
  cell* call = eval(expr);
  // (7 9 4)
  CHECK_EQ(car(lookup("a")), new_num(7));
  CHECK_EQ(car(cdr(lookup("a"))), new_num(9));
  CHECK_EQ(car(cdr(cdr(lookup("a")))), new_num(4));
  CHECK_EQ(cdr(cdr(cdr(lookup("a")))), nil);
  rmref(call);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_list_splice_inserts_shorter_list() {
  new_dynamic_scope("a", new_cons(new_num(3), new_cons(new_num(4), new_cons(new_num(5)))));   // (3 4 5)
  cell* expr = read("(list_splice a 1 3 '(6))");
  cell* call = eval(expr);
  // (3 6)
  CHECK_EQ(car(lookup("a")), new_num(3));
  CHECK_EQ(car(cdr(lookup("a"))), new_num(6));
  CHECK_EQ(cdr(cdr(lookup("a"))), nil);
  rmref(call);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_list_splice_inserts_longer_list() {
  new_dynamic_scope("a", new_cons(new_num(3), new_cons(new_num(4), new_cons(new_num(5)))));   // (3 4 5)
  cell* expr = read("(list_splice a 1 2 '(6 7 8))");
  cell* call = eval(expr);
  cell* curr = lookup("a");
  // (3 6 7 8 5)
  CHECK_EQ(car(curr), new_num(3)); curr=cdr(curr);
  CHECK_EQ(car(curr), new_num(6)); curr=cdr(curr);
  CHECK_EQ(car(curr), new_num(7)); curr=cdr(curr);
  CHECK_EQ(car(curr), new_num(8)); curr=cdr(curr);
  CHECK_EQ(car(curr), new_num(5)); curr=cdr(curr);
  CHECK_EQ(curr, nil);
  rmref(call);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_list_splice_inserts_nil() {
  new_dynamic_scope("a", new_cons(new_num(3), new_cons(new_num(4))));   // (3 4)
  cell* expr = read("(list_splice a 0 1 '(nil))");
  cell* call = eval(expr);
  // (nil 4)
  CHECK_EQ(car(lookup("a")), nil);
  CHECK_EQ(car(cdr(lookup("a"))), new_num(4));
  CHECK_EQ(cdr(cdr(lookup("a"))), nil);
  rmref(call);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_list_splice_deletes() {
  new_dynamic_scope("a", new_cons(new_num(3), new_cons(new_num(4))));   // (3 4)
  cell* expr = read("(list_splice a 1 2 nil)");
  cell* call = eval(expr);
  // (3)
  CHECK_EQ(car(lookup("a")), new_num(3));
  CHECK_EQ(cdr(lookup("a")), nil);
  rmref(call);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_list_splice_deletes_at_start() {
  new_dynamic_scope("a", new_cons(new_num(3), new_cons(new_num(4))));   // (3 4)
  cell* expr = read("(list_splice a 0 1 nil)");
  cell* call = eval(expr);
  // (4)
  CHECK_EQ(car(lookup("a")), new_num(4));
  CHECK_EQ(cdr(lookup("a")), nil);
  rmref(call);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_list_splice_deletes_entire() {
  new_dynamic_scope("a", new_cons(new_num(3), new_cons(new_num(4))));   // (3 4)
  cell* expr = read("(list_splice a 0 2 nil)");
  cell* call = eval(expr);
  CHECK_EQ(lookup("a"), nil);
  rmref(call);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_list_splice_returns_list_being_spliced_in() {
  new_dynamic_scope("a", new_cons(new_num(3), new_cons(new_num(4), new_cons(new_num(5)))));
  cell* expr = read("(list_splice a 1 2 '(6 7 8))");
  cell* result = eval(expr);
  cell* curr = result;
  // (6 7 8)
  CHECK_EQ(car(curr), new_num(6)); curr=cdr(curr);
  CHECK_EQ(car(curr), new_num(7)); curr=cdr(curr);
  CHECK_EQ(car(curr), new_num(8)); curr=cdr(curr);
  CHECK_EQ(curr, nil);
  rmref(result);
  rmref(expr);
  end_dynamic_scope("a");
}

void test_nth_cdr() {
  cell* x = new_cons(new_num(3), new_cons(new_num(4)));
  CHECK_EQ(nth_cdr(x, 0), x);
  CHECK_EQ(car(nth_cdr(x, 1)), new_num(4));
  CHECK_EQ(nth_cdr(x, 2), nil);
  rmref(x);
}

void test_contains_handles_circular_lists() {
  unordered_set<cell*> done;
  cell* x = new_cons(new_num(1));
  set_cdr(x, x);
  CHECK(!contains(x, new_sym("a"), done));
  x->cdr = nil;   // break cycle for gc
  rmref(x);
}

void test_list_sort() {
  cell* expr = read("(sort (fn(a b) (< (len a) (len b))) '(\"abc\" \"d\" \"ef\"))");
  cell* result = eval(expr);
  // ("d" "ef" "abc")
  CHECK_EQ(to_string(car(result)), "d");
  CHECK_EQ(to_string(car(cdr(result))), "ef");
  CHECK_EQ(to_string(car(cdr(cdr(result)))), "abc");
  CHECK_EQ(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(expr);
}
