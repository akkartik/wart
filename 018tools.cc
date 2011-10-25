//// Bindings for underlying functions.

COMPILE_PRIM_FUNC(addr, primFunc_addr, L"($x)",
  return mkref(newNum((long)lookup(L"$x")));
)

COMPILE_PRIM_FUNC(debug, primFunc_debug, L"($x)",
  debug = toNum(lookup(L"$x"));
  return nil;
)

COMPILE_PRIM_FUNC(inc_failures, primFunc_incTests, L"()",
  ++numFailures;
  return nil;
)

COMPILE_PRIM_FUNC(date, primFunc_date, L"()",
  time_t t = time(NULL);
  cerr << asctime(localtime(&t));
  return nil;
)

COMPILE_PRIM_FUNC(time, primFunc_time, L"'($expr)",
  clock_t start = clock();
  Cell* result = eval(lookup(L"$expr"));
  cerr << clock()-start << "/" << CLOCKS_PER_SEC << endl;
  return result;
)

COMPILE_PRIM_FUNC(quit, primFunc_quit, L"()",
  exit(0);
  return nil;
)
