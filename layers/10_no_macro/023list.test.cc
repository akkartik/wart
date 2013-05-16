void test_list_range() {
  Cell* expr = read("(list_range '(3 4 5) 0 2)");
  Cell* result = eval(expr);
  // (3 4)
  CHECK_EQ(car(result), newNum(3));
  CHECK_EQ(car(cdr(result)), newNum(4));
  CHECK_EQ(cdr(cdr(result)), nil);
  rmref(result);
  rmref(expr);
}

void test_list_splice_replaces_index() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 1 2 '(5))");
  Cell* call = eval(expr);
  // (3 5)
  CHECK_EQ(car(lookup("a")), newNum(3));
  CHECK_EQ(car(cdr(lookup("a"))), newNum(5));
  CHECK_EQ(cdr(cdr(lookup("a"))), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_replaces_first_index() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 0 1 '(9))");
  Cell* call = eval(expr);
  // (9 4)
  CHECK_EQ(car(lookup("a")), newNum(9));
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_inserts_list_at_beginning() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 0 1 '(7 9))");
  Cell* call = eval(expr);
  // (7 9 4)
  CHECK_EQ(car(lookup("a")), newNum(7));
  CHECK_EQ(car(cdr(lookup("a"))), newNum(9));
  CHECK_EQ(car(cdr(cdr(lookup("a")))), newNum(4));
  CHECK_EQ(cdr(cdr(cdr(lookup("a")))), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_inserts_shorter_list() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4), newCons(newNum(5)))));   // (3 4 5)
  Cell* expr = read("(list_splice a 1 3 '(6))");
  Cell* call = eval(expr);
  // (3 6)
  CHECK_EQ(car(lookup("a")), newNum(3));
  CHECK_EQ(car(cdr(lookup("a"))), newNum(6));
  CHECK_EQ(cdr(cdr(lookup("a"))), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_inserts_longer_list() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4), newCons(newNum(5)))));   // (3 4 5)
  Cell* expr = read("(list_splice a 1 2 '(6 7 8))");
  Cell* call = eval(expr);
  Cell* curr = lookup("a");
  // (3 6 7 8 5)
  CHECK_EQ(car(curr), newNum(3)); curr=cdr(curr);
  CHECK_EQ(car(curr), newNum(6)); curr=cdr(curr);
  CHECK_EQ(car(curr), newNum(7)); curr=cdr(curr);
  CHECK_EQ(car(curr), newNum(8)); curr=cdr(curr);
  CHECK_EQ(car(curr), newNum(5)); curr=cdr(curr);
  CHECK_EQ(curr, nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_inserts_nil() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 0 1 '(nil))");
  Cell* call = eval(expr);
  // (nil 4)
  CHECK_EQ(car(lookup("a")), nil);
  CHECK_EQ(car(cdr(lookup("a"))), newNum(4));
  CHECK_EQ(cdr(cdr(lookup("a"))), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_deletes() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 1 2 nil)");
  Cell* call = eval(expr);
  // (3)
  CHECK_EQ(car(lookup("a")), newNum(3));
  CHECK_EQ(cdr(lookup("a")), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_deletes_at_start() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 0 1 nil)");
  Cell* call = eval(expr);
  // (4)
  CHECK_EQ(car(lookup("a")), newNum(4));
  CHECK_EQ(cdr(lookup("a")), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_deletes_entire() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 0 2 nil)");
  Cell* call = eval(expr);
  CHECK_EQ(lookup("a"), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_returns_list_being_spliced_in() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4), newCons(newNum(5)))));
  Cell* expr = read("(list_splice a 1 2 '(6 7 8))");
  Cell* result = eval(expr);
  Cell* curr = result;
  // (6 7 8)
  CHECK_EQ(car(curr), newNum(6)); curr=cdr(curr);
  CHECK_EQ(car(curr), newNum(7)); curr=cdr(curr);
  CHECK_EQ(car(curr), newNum(8)); curr=cdr(curr);
  CHECK_EQ(curr, nil);
  rmref(result);
  rmref(expr);
  endDynamicScope("a");
}

void test_nthCdr() {
  Cell* x = newCons(newNum(3), newCons(newNum(4)));
  CHECK_EQ(nthCdr(x, 0), x);
  CHECK_EQ(car(nthCdr(x, 1)), newNum(4));
  CHECK_EQ(nthCdr(x, 2), nil);
  rmref(x);
}

void test_contains_handles_circular_lists() {
  unordered_set<Cell*> done;
  Cell* x = newCons(newNum(1));
  setCdr(x, x);
  CHECK(!contains(x, newSym("a"), done));
  x->cdr = nil;   // break cycle for gc
  rmref(x);
}

void test_list_sort() {
  Cell* expr = read("(sort (fn(a b) (< (len a) (len b))) '(\"abc\" \"d\" \"ef\"))");
  Cell* result = eval(expr);
  // ("d" "ef" "abc")
  CHECK_EQ(toString(car(result)), "d");
  CHECK_EQ(toString(car(cdr(result))), "ef");
  CHECK_EQ(toString(car(cdr(cdr(result)))), "abc");
  CHECK_EQ(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(expr);
}
