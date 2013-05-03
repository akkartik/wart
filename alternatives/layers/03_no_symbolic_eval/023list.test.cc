void test_list_range() {
  Cell* expr = read("(list_range '(3 4 5) 0 2)");
  Cell* result = eval(expr);
  // (3 4)
  checkEq(car(result), newNum(3));
  checkEq(car(cdr(result)), newNum(4));
  checkEq(cdr(cdr(result)), nil);
  rmref(result);
  rmref(expr);
}

void test_list_splice_replaces_index() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 1 2 '(5))");
  Cell* call = eval(expr);
  // (3 5)
  checkEq(car(lookup("a")), newNum(3));
  checkEq(car(cdr(lookup("a"))), newNum(5));
  checkEq(cdr(cdr(lookup("a"))), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_replaces_first_index() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 0 1 '(9))");
  Cell* call = eval(expr);
  // (9 4)
  checkEq(car(lookup("a")), newNum(9));
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_inserts_list_at_beginning() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 0 1 '(7 9))");
  Cell* call = eval(expr);
  // (7 9 4)
  checkEq(car(lookup("a")), newNum(7));
  checkEq(car(cdr(lookup("a"))), newNum(9));
  checkEq(car(cdr(cdr(lookup("a")))), newNum(4));
  checkEq(cdr(cdr(cdr(lookup("a")))), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_inserts_shorter_list() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4), newCons(newNum(5)))));   // (3 4 5)
  Cell* expr = read("(list_splice a 1 3 '(6))");
  Cell* call = eval(expr);
  // (3 6)
  checkEq(car(lookup("a")), newNum(3));
  checkEq(car(cdr(lookup("a"))), newNum(6));
  checkEq(cdr(cdr(lookup("a"))), nil);
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
  checkEq(car(curr), newNum(3)); curr=cdr(curr);
  checkEq(car(curr), newNum(6)); curr=cdr(curr);
  checkEq(car(curr), newNum(7)); curr=cdr(curr);
  checkEq(car(curr), newNum(8)); curr=cdr(curr);
  checkEq(car(curr), newNum(5)); curr=cdr(curr);
  checkEq(curr, nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_inserts_nil() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 0 1 '(nil))");
  Cell* call = eval(expr);
  // (nil 4)
  checkEq(car(lookup("a")), nil);
  checkEq(car(cdr(lookup("a"))), newNum(4));
  checkEq(cdr(cdr(lookup("a"))), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_deletes() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 1 2 nil)");
  Cell* call = eval(expr);
  // (3)
  checkEq(car(lookup("a")), newNum(3));
  checkEq(cdr(lookup("a")), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_deletes_at_start() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 0 1 nil)");
  Cell* call = eval(expr);
  // (4)
  checkEq(car(lookup("a")), newNum(4));
  checkEq(cdr(lookup("a")), nil);
  rmref(call);
  rmref(expr);
  endDynamicScope("a");
}

void test_list_splice_deletes_entire() {
  newDynamicScope("a", newCons(newNum(3), newCons(newNum(4))));   // (3 4)
  Cell* expr = read("(list_splice a 0 2 nil)");
  Cell* call = eval(expr);
  checkEq(lookup("a"), nil);
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
  checkEq(car(curr), newNum(6)); curr=cdr(curr);
  checkEq(car(curr), newNum(7)); curr=cdr(curr);
  checkEq(car(curr), newNum(8)); curr=cdr(curr);
  checkEq(curr, nil);
  rmref(result);
  rmref(expr);
  endDynamicScope("a");
}

void test_contains_handles_circular_lists() {
  unordered_set<Cell*> done;
  Cell* x = newCons(newNum(1));
  setCdr(x, x);
  check(!contains(x, newSym("a"), done));
  x->cdr = nil;   // break cycle for gc
  rmref(x);
}

void test_list_sort() {
  Cell* expr = read("(sort (fn(a b) (< (len a) (len b))) '(\"abc\" \"d\" \"ef\"))");
  Cell* result = eval(expr);
  // ("d" "ef" "abc")
  checkEq(toString(car(result)), "d");
  checkEq(toString(car(cdr(result))), "ef");
  checkEq(toString(car(cdr(cdr(result)))), "abc");
  checkEq(cdr(cdr(cdr(result))), nil);
  rmref(result);
  rmref(expr);
}
