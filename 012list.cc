COMPILE_PRIM_FUNC(list_splice, primFunc_list_splice, L"('$list $start $end $val)",
  Cell* binding = lookup(L"$list");
  Cell* list = eval(binding);
  long start = toNum(lookup(L"$start"));
  Cell* prePtr = nthCdr(list, start-1);
  Cell* startPtr = nthCdr(list, start);
  Cell* endPtr = nthCdr(list, toNum(lookup(L"$end")));
  Cell* val = lookup(L"$val");

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

COMPILE_PRIM_FUNC(list_get, primFunc_list_get, L"($list $index $end)",
  Cell* list = lookup(L"$list");
  int index = toNum(lookup(L"$index"));
  for (int i = 0; i < index; ++i)
    list=cdr(list);
  if (lookup(L"$end") == nil)
    return mkref(car(list));

  int end = toNum(lookup(L"$end"));
  Cell* pResult = newCell();
  Cell* curr = pResult;
  for (int i = index; i < end && list != nil; ++i, list=cdr(list), curr=cdr(curr))
    addCons(curr, car(list));
  return dropPtr(pResult);
)
