//// operations on primitive types

COMPILE_PRIM_FUNC(type, primFunc_type, L"($x)",
  return mkref(type(lookup(L"$x")));
)

COMPILE_PRIM_FUNC(iso, primFunc_iso, L"($x $y)",
  Cell* x = lookup(L"$x");
  Cell* y = lookup(L"$y");
  Cell* result = nil;
  if (x == nil && y == nil)
    result = newNum(1);
  else if (x == y)
    result = x;
  else if (isString(x) && isString(y) && toString(x) == toString(y))
    result = x;
  else
    result = nil;
  return mkref(result);
)

COMPILE_PRIM_FUNC(len, primFunc_len, L"($x)",
  Cell* x = lookup(L"$x");
  if (isString(x))
    return mkref(newNum(toString(x).length()));

  int ans = 0;
  for (; x != nil; x=cdr(x))
    ++ans;
  return mkref(newNum(ans));
)



COMPILE_PRIM_FUNC(sym, primFunc_sym, L"$args",
  ostringstream out;
  for (Cell* args = lookup(L"$args"); args != nil; args = cdr(args))
    out << car(args);
  return mkref(newSym(out.str()));
)

COMPILE_PRIM_FUNC(str, primFunc_str, L"$args",
  ostringstream out;
  for (Cell* args = lookup(L"$args"); args != nil; args = cdr(args))
    out << car(args);
  return mkref(newString(out.str()));
)

COMPILE_PRIM_FUNC(string_splice, primFunc_string_splice, L"($string $start $end $val)",
  Cell* str = lookup(L"$string");
  if (!isString(str)) {
    warn << "can't set non-string: " << str << endl;
    return nil;
  }

  size_t start = toNum(lookup(L"$start"));
  size_t end = toNum(lookup(L"$end"));
  if (start > ((string*)str->car)->length()) { // append works
    warn << "string too short: " << str << " " << start << endl;
    return nil;
  }

  Cell* val = lookup(L"$val");
  if (!isString(val))
    warn << "can't set string with non-string: " << val << endl;
  ((string*)str->car)->replace(start, end-start, toString(val));
  return mkref(val);
)

COMPILE_PRIM_FUNC(string_get, primFunc_string_get, L"($string $index)",
  Cell* str = lookup(L"$string");
  if (!isString(str)) {
    warn << "not a string: " << str << endl;
    return nil;
  }

  size_t index = toNum(lookup(L"$index"));
  if (index > ((string*)str->car)->length()-1) {
    warn << "no such index in string: " << str << " " << index << endl;
    return nil;
  }

  return mkref(newString(toString(str).substr(index, 1)));
)

COMPILE_PRIM_FUNC(list_set, primFunc_list_set, L"($list $index $val)",
  Cell* list = lookup(L"$list");
  long index = toNum(lookup(L"$index"));
  Cell* val = lookup(L"$val");
  for (long i = 0; i < index; ++i) {
    if (!isCons(list))
      warn << "can't set non-list: " << list << endl;
    list=cdr(list);
  }
  setCar(list, val);
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

COMPILE_PRIM_FUNC(table, primFunc_table, L"()",
  return mkref(newTable());
)

COMPILE_PRIM_FUNC(table_set, primFunc_table_set, L"($table $key $val)",
  Cell* table = lookup(L"$table");
  Cell* key = lookup(L"$key");
  Cell* val = lookup(L"$val");
  if (isTable(table))
    set(table, key, val);
  else
    warn << "can't set in a non-table: " << table << endl;
  return mkref(val);
)

COMPILE_PRIM_FUNC(table_get, primFunc_table_get, L"($table $key)",
  Cell* table = lookup(L"$table");
  Cell* key = lookup(L"$key");
  return mkref(get(table, key));
)



COMPILE_PRIM_FUNC(+, primFunc_add, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))+toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(-, primFunc_subtract, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))-toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(*, primFunc_multiply, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))*toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(/, primFunc_divide, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))/toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(%, primFunc_modulo, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))%toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(>, primFunc_greater, L"($x $y)",
  return toNum(lookup(L"$x")) > toNum(lookup(L"$y")) ? mkref(lookup(L"$x")) : nil;
)
