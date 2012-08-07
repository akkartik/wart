//// compiled primitives for lists

COMPILE_FN(list_range, compiledFn_list_range, "($list $index $end)",
  Cell* list = lookup("$list");
  long index = toInt(lookup("$index"));
  for (long i = 0; i < index; ++i)
    list=cdr(list);

  long end = toInt(lookup("$end"));
  Cell* pResult = newCell();
  Cell* curr = pResult;
  for (long i = index; i < end && list != nil; ++i, list=cdr(list), curr=cdr(curr))
    addCons(curr, car(list));
  return dropPtr(pResult);
)

COMPILE_FN(list_splice, compiledFn_list_splice, "('$list $start $end $val)",
  Cell* binding = lookup("$list");
  Cell* list = eval(binding);
  long start = toInt(lookup("$start"));
  Cell* prePtr = nthCdr(list, start-1);
  Cell* startPtr = nthCdr(list, start);
  Cell* endPtr = nthCdr(list, toInt(lookup("$end")));
  Cell* val = lookup("$val");

  if (val == nil) {
    if (start == 0) assign(binding, endPtr);
    else setCdr(prePtr, endPtr);
  }
  else {
    setCar(startPtr, car(val));
    mkref(endPtr);
    Cell* val2 = copyList(val);
    setCdr(startPtr, cdr(val2));
    append(startPtr, endPtr);
    rmref(val2);
    rmref(endPtr);
  }

  rmref(list);
  return mkref(val);
)



Cell* copyList(Cell* x) {
  if (!isCons(x)) return x;
  return newCons(copyList(car(x)),
      copyList(cdr(x)));
}

bool equalList(Cell* a, Cell* b) {
  if (!isCons(a)) return a == b;
  return equalList(car(a), car(b))
      && equalList(cdr(a), cdr(b));
}

Cell* nthCdr(Cell* x, long n) {
  Cell* curr = x;
  for (long idx = n; idx > 0; --idx) {
    if (!isCons(curr))
      RAISE << "list is too short: " << x << " " << n << endl;
    curr=cdr(curr);
  }
  return curr;
}

void append(Cell* x, Cell* y) {
  while(cdr(x) != nil)
    x = cdr(x);
  setCdr(x, y);
}

// useful idiom: create a dummy cell p, keep appending to it using addCons,
// then return dropPtr(p) which GC's the dummy but mkrefs the rest.
Cell* dropPtr(Cell* p) {
  Cell* x = mkref(cdr(p));
  rmref(p);
  return x;
}

void addCons(Cell* p, Cell* x) {
  setCdr(p, newCons(x));
}

// push p onto l and move one refcount to new head
Cell* pushCons(Cell* p, Cell* l) {
  Cell* result = newCons(p, l);
  rmref(cdr(result));
  return mkref(result);
}

bool contains(Cell* tree, Cell* a, unordered_set<Cell*>& done) {
  // guard against cycles
  if (done.find(tree) != done.end()) return false;
  done.insert(tree);

  if (tree == a) return true;
  if (!isCons(tree)) return false;
  return contains(car(tree), a, done)
         || contains(cdr(tree), a, done);
}

bool contains(Cell* tree, Cell* a) {
  unordered_set<Cell*> done;
  return contains(tree, a, done);
}



struct CellLt :public std::binary_function<Cell*, Cell*, bool> {
  Cell* comparer;
  CellLt(Cell* f) :comparer(f) {}
  bool operator()(Cell* a, Cell* b) {
    Cell* expr = newCons(comparer, newCons(a, newCons(b)));
    Cell* result = eval(expr);
    bool ans = (stripAlreadyEvald(result) != nil);
    rmref(result);
    rmref(expr);
    return ans;
  }
};

COMPILE_FN(sort, compiledFn_sort, "($f $list)",
  vector<Cell*> container;
  for (Cell* list = lookup("$list"); list != nil; list=cdr(list))
    container.push_back(car(list));

  std::stable_sort(container.begin(), container.end(), CellLt(lookup("$f")));

  Cell* pNewList = newCell();
  vector<Cell*>::iterator p;
  Cell* curr = pNewList;
  for (p=container.begin(); p != container.end(); ++p, curr=cdr(curr))
    addCons(curr, *p);
  return dropPtr(pNewList);
)
