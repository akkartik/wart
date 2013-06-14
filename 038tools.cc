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
  Hide_raises = true;
  return nil;
)

COMPILE_FN(show_warnings, compiledfn_show_warnings, "()",
  Hide_raises = false;
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
  bool old_count_raise = Hide_raises;
  Hide_raises = true;
    cell* ans = compiledfn_eval();
  Hide_raises = old_count_raise;

  if (Num_raises == 0) return ans;
  // error
  Num_raises = 0;
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

COMPILE_FN(trace, compiledfn_trace, "($layer '$expr)",
  START_TRACING_UNTIL_END_OF_SCOPE;
  Trace_stream->dump_layer = to_string(lookup("$layer"));
  return eval(lookup("$expr"));
)
