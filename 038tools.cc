//// bindings for underlying functions

COMPILE_FN(addr, compiledfn_addr, "($x)",
  return mkref(new_num((long)lookup("$x")));
)

COMPILE_FN(register_failed_test, compiledfn_register_failed_test, "($msg $expr $expected)",
  ++Num_failures;
  cout << "\nF ";  print(lookup("$msg"), cout);
  cout << "\n  got " << lookup("$expr") << '\n';
  cout << "  expected " << lookup("$expected") << '\n';
  return nil;
)

COMPILE_FN(hiding_warnings, compiledfn_hide_warnings, "'$body",
  START_TRACING_UNTIL_END_OF_SCOPE;
  Hide_warnings = true;
  for (cell* body = lookup("$body"); body != nil; body=cdr(body))
    rmref(eval(car(body)));
  Hide_warnings = false;
  return nil;
)

COMPILE_FN(mem_usage, compiledfn_mem_usage, "()",
  return mkref(new_num(num_unfreed()));
)

COMPILE_FN(nrefs, compiledfn_nrefs, "($x)",
  return mkref(new_num(lookup("$x")->nrefs));
)

COMPILE_FN(date, compiledfn_date, "()",
  string buf(time_string());
  if (!buf.empty())
    cerr << buf << ' ';
  else
    cerr << "strftime buffer too small\n";
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
  bool old_count_raise = Hide_warnings;
  Hide_warnings = true;
    cell* ans = compiledfn_eval();
  Hide_warnings = old_count_raise;

  if (trace_count("warn") == 0) return ans;
  // error
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
