COMPILE_FN(list_splice, compiledFn_list_splice, "('$list $start $end $val)",
  Cell* binding = lookup("$list");
  Cell* list = eval(binding);
  long start = toNum(lookup("$start"));
  Cell* prePtr = nthCdr(list, start-1);
  Cell* startPtr = nthCdr(list, start);
  Cell* endPtr = nthCdr(list, toNum(lookup("$end")));
  Cell* val = lookup("$val");

  if (val == nil) {
    if (start == 0)
      assign(binding, endPtr);
    else
      setCdr(prePtr, endPtr);
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

  if (cdr(val) == nil) val = car(val);
  rmref(list);
  return mkref(val);
)

COMPILE_FN(list_range, compiledFn_list_range, "($list $index $end)",
  Cell* list = lookup("$list");
  int index = toNum(lookup("$index"));
  for (int i = 0; i < index; ++i)
    list=cdr(list);

  int end = toNum(lookup("$end"));
  Cell* pResult = newCell();
  Cell* curr = pResult;
  for (int i = index; i < end && list != nil; ++i, list=cdr(list), curr=cdr(curr))
    addCons(curr, car(list));
  return dropPtr(pResult);
)

                                  struct CellLt :public std::binary_function<Cell*, Cell*, bool> {
                                    Cell* comparer;
                                    CellLt(Cell* f) :comparer(f) {}
                                    bool operator()(Cell* a, Cell* b) {
                                      Cell* expr = newCons(comparer, newCons(a, newCons(b, nil)));
                                      Cell* result = eval(expr);
                                      bool ans = (result != nil);
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
