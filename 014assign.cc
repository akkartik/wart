COMPILE_PRIM_FUNC(assign, L"('x y)",
  result = lookup(L"y");
  Cell* x = lookup(L"x");
  if (dynamics[(long)x].empty())
    newDynamicScope(x, result);
  else
    assignDynamicVar(x, result);
)
