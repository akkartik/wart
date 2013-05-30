//// bindings for underlying functions

COMPILE_FN(addr, compiledfn_addr, "($x)",
  return mkref(new_num((long)lookup("$x")));
)

COMPILE_FN(register_failed_test, compiledfn_register_failed_test, "($msg $expr)",
  ++Num_failures;
  cout << "\nF "; print(lookup("$msg"), cout);
  cout << "\n  got " << lookup("$expr") << '\n';
  return nil;
)

COMPILE_FN(hide_warnings, compiledfn_hide_warnings, "()",
  Do_raise = false;
  return nil;
)

COMPILE_FN(show_warnings, compiledfn_show_warnings, "()",
  Do_raise = true;
  return nil;
)

COMPILE_FN(mem_usage, compiledfn_mem_usage, "()",
  return mkref(new_num(num_unfreed()));
)

COMPILE_FN(nrefs, compiledfn_nrefs, "($x)",
  return mkref(new_num(lookup("$x")->nrefs));
)

COMPILE_FN(date, compiledfn_date, "()",
  time_t t = time(NULL);
  cerr << asctime(localtime(&t));
  return nil;
)

COMPILE_FN(time, compiledfn_time, "('$expr)",
  clock_t start = clock();
  cell* result = eval(lookup("$expr"));
  cerr << clock()-start << "/" << CLOCKS_PER_SEC << '\n';
  return result;  // already mkref'd
)

COMPILE_FN(exit, compiledfn_exit, "($status)",
  cell* s = lookup("$status");
  exit(is_num(s) ? to_int(s) : -1);
  return nil;
)

COMPILE_FN(globals, compiledfn_globals, "()",
  cell* ans = new_table();
  for (unordered_map<cell*, stack<cell*> >::iterator p = Dynamics.begin(); p != Dynamics.end(); ++p)
    if (!p->second.empty())
      set(ans, p->first, p->second.top());
  return mkref(ans);
)

// eval in a sandbox
COMPILE_FN(try_eval, compiledfn_try_eval, "($x ... $scope)",
  bool old_raise = Do_raise;
  Do_raise = false;
    cell* ans = compiledfn_eval();
  Do_raise = old_raise;

  if (Raise_count == 0) return ans;
  // error
  Raise_count = 0;
  rmref(ans);
  return nil;
)

COMPILE_FN(tmpfile, compiledfn_tmpfile, "()",
  char tmp[] = "/tmp/wart_XXXXXX";
  long fd = mkstemp(tmp);
  if (fd == -1) return nil;
  close(fd);
  return mkref(new_string(tmp));
)
