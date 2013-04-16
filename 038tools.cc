//// bindings for underlying functions

COMPILE_FN(addr, compiledFn_addr, "($x)",
  return mkref(newNum((long)lookup("$x")));
)

COMPILE_FN(register_failed_test, compiledFn_incTests, "($msg $expr)",
  ++numFailures;
  cout << endl << "F "; print(lookup("$msg"), cout);
  cout << endl << "  got " << lookup("$expr") << endl;
  return nil;
)

COMPILE_FN(hide_warnings, compiledFn_hide_warnings, "()",
  pretendRaise = true;
  return nil;
)

COMPILE_FN(show_warnings, compiledFn_show_warnings, "()",
  pretendRaise = false;
  return nil;
)

COMPILE_FN(mem_usage, compiledFn_mem_usage, "()",
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

COMPILE_FN(time, compiledFn_time, "('$expr)",
  clock_t start = clock();
  Cell* result = eval(lookup("$expr"));
  cerr << clock()-start << "/" << CLOCKS_PER_SEC << endl;
  return result;  // already mkref'd
)

COMPILE_FN(exit, compiledFn_exit, "($status)",
  Cell* s = lookup("$status");
  exit(isNum(s) ? toInt(s) : -1);
  return nil;
)

COMPILE_FN(globals, compiledFn_globals, "()",
  Cell* ans = newTable();
  for (unordered_map<Cell*, stack<Cell*> >::iterator p = dynamics.begin(); p != dynamics.end(); ++p)
    if (!p->second.empty())
      set(ans, p->first, p->second.top());
  return mkref(ans);
)

// eval in a sandbox
COMPILE_FN(try_eval, compiledFn_try_eval, "($x ... $scope)",
  bool oldPretendRaise = pretendRaise;
  pretendRaise = true;
    Cell* ans = compiledFn_eval();
  pretendRaise = oldPretendRaise;

  if (raiseCount == 0) return ans;
  // error
  raiseCount = 0;
  rmref(ans);
  return nil;
)

COMPILE_FN(tmpfile, compiledFn_tmpfile, "()",
  char tmp[] = "/tmp/wart_XXXXXX";
  long fd = mkstemp(tmp);
  if (fd == -1) return nil;
  close(fd);
  return mkref(newString(tmp));
)
