COMPILE_PRIM_FUNC(cons, L"(x y)",
  result = newCell();
  setCar(result, lookup(L"x"));
  setCdr(result, lookup(L"y"));
)

COMPILE_PRIM_FUNC(car, L"(x)",
  result = car(lookup(L"x"));
)

COMPILE_PRIM_FUNC(cdr, L"(x)",
  result = cdr(lookup(L"x"));
)

COMPILE_PRIM_FUNC(_isCons, L"(x)",
  result = lookup(L"x");
  if (!isCons(result))
    result = nil;
)

COMPILE_PRIM_FUNC(_isNil, L"(x)",
  result = lookup(L"x");
  if (result == nil)
    result = newSym(L"t");
  else
    result = nil;
)

COMPILE_PRIM_FUNC(assign, L"('x y)",
  result = lookup(L"y");
  Cell* x = lookup(L"x");
  if (dynamics[(long)x].empty())
    newDynamicScope(x, result);
  else
    assignDynamicVar(x, result);
)

COMPILE_PRIM_FUNC(eval, L"(x)",
  result = eval(lookup(L"x"));
)

// HACK because there's no wifstream(wstring) constructor
// will only work with strings containing ascii characters
vector<ascii> toAscii(string s) {
  vector<ascii> result;
  for (string::iterator p = s.begin(); p != s.end(); ++p)
    result.push_back(*p);
  return result;
}

COMPILE_PRIM_FUNC(load, L"(f)",
  loadFile(&toAscii(toString(lookup(L"f")))[0]);
)

COMPILE_PRIM_FUNC(_prn, L"(x)",
  result = lookup(L"x");
  cout << result << endl;
)

COMPILE_PRIM_FUNC(_if, L"(cond 'then 'else)",
  if (lookup(L"cond") != nil)
    result = eval(lookup(L"then"));
  else
    result = eval(lookup(L"else"));
)

COMPILE_PRIM_FUNC(_atom_equal, L"(x y)",
  Cell* x = lookup(L"x");
  Cell* y = lookup(L"y");
  if (x == y)
    result = x;
  else if (isString(x) && isString(y) && toString(x) == toString(y))
    result = x;
  else
    result = nil;
)
