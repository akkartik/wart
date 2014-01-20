//// compiled primitives for lists

COMPILE_FN(cons?, compiledfn_is_cons, "($x)",
  cell* x = lookup("$x");
  if (!is_cons(x)) return nil;
  return mkref(x);
)

COMPILE_FN(cons, compiledfn_cons, "($x $y)",
  return mkref(new_cons(lookup("$x"), lookup("$y")));
)

COMPILE_FN(car, compiledfn_car, "($l)",
  return mkref(car(lookup("$l")));
)

COMPILE_FN(cdr, compiledfn_cdr, "($l)",
  return mkref(cdr(lookup("$l")));
)

COMPILE_FN(set_car, compiledfn_set_car, "($cons $val)",
  set_car(lookup("$cons"), lookup("$val"));
  return mkref(lookup("$val"));
)

COMPILE_FN(set_cdr, compiledfn_set_cdr, "($cons $val)",
  set_cdr(lookup("$cons"), lookup("$val"));
  return mkref(lookup("$val"));
)

COMPILE_FN(len, compiledfn_len, "($x)",
  return mkref(new_num(len(lookup("$x"))));
)

long len(cell* l) {
  if (is_string(l))
    return to_string(l).length();
  long ans = 0;
  for (; l != nil; l=cdr(l))
    ++ans;
  return ans;
}

COMPILE_FN(list_range, compiledfn_list_range, "($list $index $end)",
  cell* list = lookup("$list");
  long index = to_int(lookup("$index"));
  for (long i = 0; i < index; ++i)
    list=cdr(list);

  long end = to_int(lookup("$end"));
  cell* p_result = new_cell();
  cell* curr = p_result;
  for (long i = index; i < end && list != nil; ++i, list=cdr(list), curr=cdr(curr))
    add_cons(curr, car(list));
  return drop_ptr(p_result);
)

COMPILE_FN(list_splice, compiledfn_list_splice, "('$list $start $end $val)",
  cell* binding = lookup("$list");
  TEMP(list, eval(binding));
  long start = to_int(lookup("$start"));
  cell* pre_ptr = nth_cdr(list, start-1);
  cell* start_ptr = nth_cdr(list, start);
  TEMP(end_ptr, mkref(nth_cdr(list, to_int(lookup("$end")))));
  cell* val = lookup("$val");

  if (val == nil) {
    if (start == 0) assign(binding, end_ptr);
    else set_cdr(pre_ptr, end_ptr);
  }
  else {
    set_car(start_ptr, car(val));
    TEMP(val2, mkref(copy_list(val)));
    set_cdr(start_ptr, cdr(val2));
    append(start_ptr, end_ptr);
  }

  return mkref(val);
)



cell* copy_list(cell* x) {
  if (!is_cons(x)) return x;
  return new_cons(copy_list(car(x)),
      copy_list(cdr(x)));
}

bool equal_list(cell* a, cell* b) {
  if (!is_cons(a)) return a == b;
  return equal_list(car(a), car(b))
      && equal_list(cdr(a), cdr(b));
}

cell* nth_cdr(cell* x, long n) {
  cell* curr = x;
  for (long idx = n; idx > 0; --idx) {
    if (!is_cons(curr))
      RAISE << "list is too short: " << x << " " << n << '\n';
    curr=cdr(curr);
  }
  return curr;
}

void append(cell* x, cell* y) {
  if (y == nil) return;
  while(cdr(x) != nil)
    x = cdr(x);
  set_cdr(x, y);
}

// useful idiom: create a dummy cell p, keep appending to it using add_cons,
// then return drop_ptr(p) which GC's the dummy but mkrefs the rest.
cell* drop_ptr(cell* p) {
  cell* x = mkref(cdr(p));
  if (p->nrefs == 0) ++p->nrefs;  // suppress rmref warning
  rmref(p);
  return x;
}

void add_cons(cell* p, cell* x) {
  set_cdr(p, new_cons(x));
}

// push p onto l and move one refcount to new head
cell* push_cons(cell* p, cell* l) {
  cell* result = new_cons(p, l);
  rmref(cdr(result));
  return mkref(result);
}

bool contains(cell* tree, cell* a, unordered_set<cell*>& done) {
  // guard against cycles
  if (done.find(tree) != done.end()) return false;
  done.insert(tree);

  if (tree == a) return true;
  if (!is_cons(tree)) return false;
  return contains(car(tree), a, done)
         || contains(cdr(tree), a, done);
}

bool contains(cell* tree, cell* a) {
  unordered_set<cell*> done;
  return contains(tree, a, done);
}



struct cell_lt_comparator :public std::binary_function<cell*, cell*, bool> {
  cell* comparer;
  cell_lt_comparator(cell* f) :comparer(f) {}
  bool operator()(cell* a, cell* b) {
    // quote args of call; they've already been eval'd inside sort
    TEMP(expr, mkref(new_cons(comparer, new_cons(new_cons(sym_quote, a),
                                                 new_cons(new_cons(sym_quote, b))))));
    TEMP(result, eval(expr));
    return result != nil && result != sym_false;
  }
};

COMPILE_FN(sort, compiledfn_sort, "($f $list)",
  vector<cell*> container;
  for (cell* list = lookup("$list"); list != nil; list=cdr(list))
    container.push_back(car(list));

  std::stable_sort(container.begin(), container.end(), cell_lt_comparator(lookup("$f")));

  cell* p_new_list = new_cell();
  vector<cell*>::iterator p;
  cell* curr = p_new_list;
  for (p=container.begin(); p != container.end(); ++p, curr=cdr(curr))
    add_cons(curr, *p);
  return drop_ptr(p_new_list);
)
