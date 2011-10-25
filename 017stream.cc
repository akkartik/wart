COMPILE_PRIM_FUNC(sym, primFunc_sym, L"$args",
  ostringstream out;
  for (Cell* args = lookup(L"$args"); args != nil; args = cdr(args))
    if (isString(car(args))) out << toString(car(args));
    else out << car(args);
  return mkref(newSym(out.str()));
)
