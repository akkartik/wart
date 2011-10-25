COMPILE_PRIM_FUNC(sym, primFunc_sym, L"$args",
  ostringstream out;
  for (Cell* args = lookup(L"$args"); args != nil; args = cdr(args))
    if (isString(car(args))) out << toString(car(args));
    else out << car(args);
  return mkref(newSym(out.str()));
)

COMPILE_PRIM_FUNC(pr, primFunc_prn, L"($x)",
  Cell* x = lookup(L"$x");
  if (isString(x)) cout << toString(x);
  else cout << x;
  printDepth=0;
  cout.flush();
  return mkref(x);
)
