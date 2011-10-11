//// Compiled primitives

// primFuncs take $vars as params.
// $vars needn't turn into gensyms if primFuncs don't call each other.

COMPILE_PRIM_FUNC(eval, primFunc_eval, L"($x)",
  return eval(lookup(L"$x"));
)

COMPILE_PRIM_FUNC(cons, primFunc_cons, L"($x $y)",
  return mkref(newCons(lookup(L"$x"), lookup(L"$y")));
)

COMPILE_PRIM_FUNC(car, primFunc_car, L"($l)",
  return mkref(car(lookup(L"$l")));
)

COMPILE_PRIM_FUNC(cdr, primFunc_cdr, L"($l)",
  return mkref(cdr(lookup(L"$l")));
)

COMPILE_PRIM_FUNC(not, primFunc_not, L"($x)",
  return lookup(L"$x") == nil ? mkref(newNum(1)) : nil;
)

COMPILE_PRIM_FUNC(assign, primFunc_assign, L"('$var $val)",
  Cell* var = lookup(L"$var");
  Cell* val = lookup(L"$val");
  Cell* currLexicalScope = currLexicalScopes.top();
  if (isCons(currLexicalScope))
    currLexicalScope = car(currLexicalScope);
  Cell* scope = scopeContainingBinding(var, currLexicalScope);
  if (!scope)
    newDynamicScope(var, val);
  else if (scope == nil)
    assignDynamicVar(var, val);
  else
    unsafeSet(scope, var, val, false);
  return mkref(val);
)

COMPILE_PRIM_FUNC(if, primFunc_if, L"($cond '$then '$else)",
  return lookup(L"$cond") != nil ? eval(lookup(L"$then")) : eval(lookup(L"$else"));
)

COMPILE_PRIM_FUNC(addr, primFunc_addr, L"($x)",
  return mkref(newNum((long)lookup(L"$x")));
)

COMPILE_PRIM_FUNC(iso, primFunc_iso, L"($x $y)",
  Cell* x = lookup(L"$x");
  Cell* y = lookup(L"$y");
  Cell* result = nil;
  if (x == nil && y == nil)
    result = newNum(1);
  else if (x == y)
    result = x;
  else if (isString(x) && isString(y) && toString(x) == toString(y))
    result = x;
  else
    result = nil;
  return mkref(result);
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
  cout << x;
  cout.flush();
  return mkref(x);
)

COMPILE_PRIM_FUNC(debug, primFunc_debug, L"($x)",
  debug = toNum(lookup(L"$x"));
  return nil;
)

COMPILE_PRIM_FUNC(incFailures, primFunc_incTests, L"()",
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



COMPILE_PRIM_FUNC(uniq, primFunc_uniq, L"($x)",
  return mkref(genSym(lookup(L"$x")));
)

COMPILE_PRIM_FUNC(sym, primFunc_sym, L"$args",
  ostringstream out;
  for (Cell* args = lookup(L"$args"); args != nil; args = cdr(args))
    out << car(args);
  return mkref(newSym(out.str()));
)

COMPILE_PRIM_FUNC(str, primFunc_str, L"$args",
  ostringstream out;
  for (Cell* args = lookup(L"$args"); args != nil; args = cdr(args))
    out << car(args);
  return mkref(newString(out.str()));
)

COMPILE_PRIM_FUNC(table, primFunc_table, L"()",
  return mkref(newTable());
)

COMPILE_PRIM_FUNC(table_set, primFunc_table_set, L"($table $key $val)",
  Cell* table = lookup(L"$table");
  Cell* key = lookup(L"$key");
  Cell* val = lookup(L"$val");
  if (isTable(table))
    set(table, key, val);
  else
    warn << "can't set in a non-table: " << table << endl;
  return mkref(val);
)

COMPILE_PRIM_FUNC(table_get, primFunc_table_get, L"($table $key)",
  Cell* table = lookup(L"$table");
  Cell* key = lookup(L"$key");
  return mkref(get(table, key));
)

COMPILE_PRIM_FUNC(type, primFunc_type, L"($x)",
  Cell* x = lookup(L"$x");
  Cell* result = nil;
  if (x != nil)
    switch(x->type) {
    case NUM:
      result = newSym(L"number"); break;
    case SYM:
      result = newSym(L"symbol"); break;
    case STRING:
      result = newSym(L"string"); break;
    case TABLE:
      result = newSym(L"table"); break;
    case PRIM_FUNC:
      result = newSym(L"function"); break;
    case CONS:
      if (car(x) == newSym(L"fn") || car(x) == newSym(L"evald-fn"))
        result = newSym(L"function");
      else if (car(x) == newSym(L"type"))
        result = car(cdr(x));
      else
        result = newSym(L"list");
      break;
    default:
      err << "Undefined type: " << x->type << endl << DIE;
    }
  return mkref(result);
)



COMPILE_PRIM_FUNC(+, primFunc_add, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))+toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(-, primFunc_subtract, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))-toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(*, primFunc_multiply, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))*toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(/, primFunc_divide, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))/toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(%, primFunc_modulo, L"($x $y)",
  return mkref(newNum(toNum(lookup(L"$x"))%toNum(lookup(L"$y"))));
)

COMPILE_PRIM_FUNC(>, primFunc_greater, L"($x $y)",
  return toNum(lookup(L"$x")) > toNum(lookup(L"$y")) ? mkref(lookup(L"$x")) : nil;
)
