//// Bindings for underlying functions.

COMPILE_PRIM_FUNC(addr, primFunc_addr, "($x)",
  return mkref(newNum((long)lookup("$x")));
)

COMPILE_PRIM_FUNC(debug, primFunc_debug, "($x)",
  debug = toNum(lookup("$x"));
  return nil;
)

COMPILE_PRIM_FUNC(register_failed_test, primFunc_incTests, "($msg $expr)",
  ++numFailures;
  cout << endl << "F "; print(lookup("$msg"), cout);
  cout << endl << "  got " << lookup("$expr") << endl;
  return nil;
)

COMPILE_PRIM_FUNC(mem_usage, primFunc_mem_usage, "()",
  long numUnfreed();
  return mkref(newNum(numUnfreed()));
)

COMPILE_PRIM_FUNC(recent_allocs, primFunc_recent_allocs, "()",
  for (int i = (recentAllocsIdx+1)%NRECENT; i != recentAllocsIdx; i = (i+1)%NRECENT)
    if (recentAllocs[i] && recentAllocs[i]->car)
      cout << recentAllocs[i] << endl;
  return nil;
)

COMPILE_PRIM_FUNC(date, primFunc_date, "()",
  time_t t = time(NULL);
  cerr << asctime(localtime(&t));
  return nil;
)

COMPILE_PRIM_FUNC(time, primFunc_time, "'($expr)",
  clock_t start = clock();
  Cell* result = eval(lookup("$expr"));
  cerr << clock()-start << "/" << CLOCKS_PER_SEC << endl;
  return result;
)

COMPILE_PRIM_FUNC(quit, primFunc_quit, "()",
  exit(0);
  return nil;
)
