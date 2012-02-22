//// Bindings for underlying functions.

COMPILE_FN(addr, compiledFn_addr, "($x)",
  return mkref(newNum((long)lookup("$x")));
)

COMPILE_FN(debug, compiledFn_debug, "($x)",
  debug = toNum(lookup("$x"));
  return nil;
)

COMPILE_FN(register_failed_test, compiledFn_incTests, "($msg $expr)",
  ++numFailures;
  cout << endl << "F "; print(lookup("$msg"), cout);
  cout << endl << "  got " << lookup("$expr") << endl;
  return nil;
)

COMPILE_FN(mem_usage, compiledFn_mem_usage, "()",
  long numUnfreed();
  return mkref(newNum(numUnfreed()));
)

COMPILE_FN(nrefs, compiledFn_nrefs, "($x)",
  return mkref(newNum(lookup("$x")->nrefs));
)

COMPILE_FN(date, compiledFn_date, "()",
  time_t t = time(NULL);
  cerr << asctime(localtime(&t));
  return nil;
)

COMPILE_FN(time, compiledFn_time, "'($expr)",
  clock_t start = clock();
  Cell* result = eval(lookup("$expr"));
  cerr << clock()-start << "/" << CLOCKS_PER_SEC << endl;
  return result; // already mkref'd
)

COMPILE_FN(quit, compiledFn_quit, "()",
  exit(0);
  return nil;
)

COMPILE_FN(globals, compiledFn_globals, "()",
  Cell* ans = newTable();
  for (unordered_map<Cell*, stack<Cell*> >::iterator p = dynamics.begin(); p != dynamics.end(); ++p)
    if (!p->second.empty())
      set(ans, p->first, p->second.top());
  return mkref(ans);
)
