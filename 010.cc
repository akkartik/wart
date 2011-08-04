COMPILE_PRIM_FUNC(cons, L"(x y)",
  result = newCell();
  setCar(result, lookup(L"x"));
  setCdr(result, lookup(L"y"));
  mkref(result);
)

COMPILE_PRIM_FUNC(car, L"(x)",
  result = car(lookup(L"x"));
  mkref(result);
)

COMPILE_PRIM_FUNC(cdr, L"(x)",
  result = cdr(lookup(L"x"));
  mkref(result);
)

COMPILE_PRIM_FUNC(_isCons, L"(x)",
  result = lookup(L"x");
  if (!isCons(result))
    result = nil;
  mkref(result);
)

COMPILE_PRIM_FUNC(_isNil, L"(x)",
  result = lookup(L"x");
  if (result == nil)
    result = newNum(1);
  else
    result = nil;
  mkref(result);
)

COMPILE_PRIM_FUNC(assign, L"('x y)",
  result = lookup(L"y");
  Cell* x = lookup(L"x");
  if (dynamics[(long)x].empty())
    newDynamicScope(x, result);
  else
    assignDynamicVar(x, result);
  mkref(result);
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
  mkref(result);
)

COMPILE_PRIM_FUNC(_if, L"'(cond then else)",
  Cell* cond = lookup(L"cond");
  Cell* then = lookup(L"then");
  Cell* rest = lookup(L"else");
  // now evaluate in caller's lexical scope
  endLexicalScope();
  endDynamicScope(L"currLexicalScope");
  cond = eval(cond);
  if (cond != nil)
    result = eval(then);
  else
    result = eval(rest);
  rmref(cond);
  newDynamicScope(L"currLexicalScope", nil);
  newLexicalScope();
)

COMPILE_PRIM_FUNC(_atom_equal, L"(x y)",
  Cell* x = lookup(L"x");
  Cell* y = lookup(L"y");
  if (x == nil && y == nil)
    result = newNum(1);
  else if (x == y)
    result = x;
  else if (isString(x) && isString(y) && toString(x) == toString(y))
    result = x;
  else
    result = nil;
  mkref(result);
)

COMPILE_PRIM_FUNC(debug, L"(x)",
  debug = toNum(lookup(L"x"));
)

COMPILE_PRIM_FUNC(uniq, L"(x)",
  static long counter = 0;
  Cell* x = lookup(L"x");
  ostringstream os;
  os << (x == nil ? L"sym" : toString(x)) << ++counter;
  result = newSym(os.str());
  mkref(result);
)
