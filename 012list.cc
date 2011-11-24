COMPILE_PRIM_FUNC(list_splice, primFunc_list_splice, "('$list $start $end $val)",
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

COMPILE_PRIM_FUNC(list_range, primFunc_list_range, "($list $index $end)",
  Cell* list = lookup("$list");
  int index = toNum(lookup("$index"));
  for (int i = 0; i < index; ++i)
    list=cdr(list);
  if (lookup("$end") == nil)
    return mkref(car(list));

  int end = toNum(lookup("$end"));
  Cell* pResult = newCell();
  Cell* curr = pResult;
  for (int i = index; i < end && list != nil; ++i, list=cdr(list), curr=cdr(curr))
    addCons(curr, car(list));
  return dropPtr(pResult);
)
