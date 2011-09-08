COMPILE_PRIM_FUNC(eval, primFunc_eval,
  Cell* x = eval(car(args));
  Cell* result = eval(x);
  rmref(x);
  return result; // already mkref'd
)

COMPILE_PRIM_FUNC(cons, primFunc_cons,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = newCell();
  setCar(result, x);
  setCdr(result, y);
  rmref(x);
  rmref(y);
  return mkref(result);
)

COMPILE_PRIM_FUNC(car, primFunc_car,
  Cell* x = eval(car(args));
  Cell* result = car(x);
  rmref(x);
  return mkref(result);
)

COMPILE_PRIM_FUNC(cdr, primFunc_cdr,
  Cell* x = eval(car(args));
  Cell* result = mkref(cdr(x));
  rmref(x);
  return result; // already mkref'd
)

COMPILE_PRIM_FUNC(cons?, primFunc_isCons,
  Cell* x = eval(car(args));
  if (isCons(x))
    return x; // already mkref'd
  rmref(x);
  return nil;
)

COMPILE_PRIM_FUNC(not, primFunc_not,
  Cell* x = eval(car(args));
  Cell* result = (x == nil ? newNum(1) : nil);
  rmref(x);
  return mkref(result);
)

COMPILE_PRIM_FUNC(assign, primFunc_assign,
  Cell* var = car(args);
  Cell* val = eval(car(cdr(args)));
  Cell* currLexicalScope = currLexicalScopes.top();
  if (isCons(currLexicalScope))
    currLexicalScope = car(currLexicalScope);
  Cell* scope = scopeContainingBinding(var, currLexicalScope);
  if (!scope)
    newDynamicScope(var, val);
  else if (scope != newSym(L"dynamicScope"))
    unsafeSet(currLexicalScopes.top(), var, val, false);
  else
    assignDynamicVar(var, val);
  return val; // already mkref'd
)

                                  // HACK because there's no wifstream(wstring) constructor
                                  // will only work with strings containing ascii characters
                                  vector<ascii> toAscii(string s) {
                                    vector<ascii> result;
                                    for (string::iterator p = s.begin(); p != s.end(); ++p)
                                      result.push_back(*p);
                                    return result;
                                  }

COMPILE_PRIM_FUNC(load, primFunc_load,
  Cell* f = eval(car(args));
  loadFile(&toAscii(toString(f))[0]);
  return nil;
)

COMPILE_PRIM_FUNC(prn, primFunc_prn,
  Cell* x = eval(car(args));
  cout << x << endl;
  return x; // already mkref'd
)

COMPILE_PRIM_FUNC(if, primFunc_if,
  Cell* cond = eval(car(args));
  Cell* then = car(cdr(args));
  Cell* rest = car(cdr(cdr(args)));
  Cell* result = (cond != nil) ? eval(then) : eval(rest);
  rmref(cond);
  return result; // already mkref'd
)

COMPILE_PRIM_FUNC(atom_equal, primFunc_atom_equal,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
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

COMPILE_PRIM_FUNC(debug, primFunc_debug,
  debug = toNum(car(args));
  return nil;
)

COMPILE_PRIM_FUNC(uniq, primFunc_uniq,
  Cell* x = eval(car(args));
  Cell* result = genSym(x);
  rmref(x);
  return mkref(result);
)

COMPILE_PRIM_FUNC(sym, primFunc_sym,
  ostringstream out;
  Cell* arg = NULL;
  for (; args != nil; args = cdr(args)) {
    arg = eval(car(args));
    out << arg;
    rmref(arg);
  }
  return mkref(newSym(out.str()));
)

COMPILE_PRIM_FUNC(table, primFunc_table,
  args = args; // ignore warning
  return mkref(newTable());
)

COMPILE_PRIM_FUNC(table_set, primFunc_table_set,
  Cell* table = eval(car(args));
  Cell* key = eval(car(cdr(args)));
  Cell* val = eval(car(cdr(cdr(args))));
  if (isTable(table))
    set(table, key, val);
  rmref(table);
  rmref(key);
  return val; // already mkref'd
)

COMPILE_PRIM_FUNC(type, primFunc_type,
  Cell* x = eval(car(args));
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
      if (car(x) == newSym(L"lambda") || car(x) == newSym(L"evald-lambda"))
        result = newSym(L"function");
      else if (car(x) == newSym(L"type"))
        result = car(cdr(x));
      else
        result = newSym(L"list");
      break;
    default:
      err << "Undefined type: " << x->type << endl << DIE;
    }
  rmref(x);
  return mkref(result);
)
