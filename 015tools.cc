//// Bindings for underlying functions.

COMPILE_PRIM_FUNC(addr, primFunc_addr, L"($x)",
  return mkref(newNum((long)lookup(L"$x")));
)

                                  // HACK because there's no wifstream(wstring) constructor
                                  // will only work with strings containing ascii characters
                                  vector<ascii> toAscii(string s) {
                                    vector<ascii> result;
                                    for (string::iterator p = s.begin(); p != s.end(); ++p)
                                      result.push_back(*p);
                                    return result;
                                  }

COMPILE_PRIM_FUNC(load, primFunc_load, L"($f)",
  loadFile(&toAscii(toString(lookup(L"$f")))[0]);
  return nil;
)

COMPILE_PRIM_FUNC(pr, primFunc_prn, L"($x)",
  Cell* x = lookup(L"$x");
  cout << x; printDepth=0;
  cout.flush();
  return mkref(x);
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
