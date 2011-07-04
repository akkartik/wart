COMPILE_PRIM_FUNC(_prn, L"(x)",
  result = eval(lookup(L"x"));
  cout << result << endl;
)
