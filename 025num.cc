//// compiled primitives for numbers

COMPILE_FN(+, compiledfn_add, "($x $y)",
  cell* x = lookup("$x"); cell* y = lookup("$y");
  if (x->type == FLOAT || y->type == FLOAT)
    return mkref(new_num(to_float(x) + to_float(y)));
  else
    return mkref(new_num(to_int(x) + to_int(y)));
)

COMPILE_FN(-, compiledfn_subtract, "($x $y)",
  cell* x = lookup("$x"); cell* y = lookup("$y");
  if (x->type == FLOAT || y->type == FLOAT)
    return mkref(new_num(to_float(x) - to_float(y)));
  else
    return mkref(new_num(to_int(x) - to_int(y)));
)

COMPILE_FN(*, compiledfn_multiply, "($x $y)",
  cell* x = lookup("$x"); cell* y = lookup("$y");
  if (x->type == FLOAT || y->type == FLOAT)
    return mkref(new_num(to_float(x) * to_float(y)));
  else
    return mkref(new_num(to_int(x) * to_int(y)));
)

COMPILE_FN(/, compiledfn_divide, "($x $y)",
  cell* x = lookup("$x"); cell* y = lookup("$y");
  return mkref(new_num(to_float(x) / to_float(y)));
)

COMPILE_FN(%, compiledfn_modulo, "($x $y)",
  return mkref(new_num(to_int(lookup("$x")) % to_int(lookup("$y"))));  // what does modulo of floats mean?
)

COMPILE_FN(<, compiledfn_lesser, "($x $y)",
  cell* x = lookup("$x");
  cell* y = lookup("$y");
  if (x == nil || y == nil) return nil;
  return to_float(x) < to_float(y) ? mkref(y) : nil;
)

COMPILE_FN(int, compiledfn_integer, "($x)",
  return mkref(new_num(to_int(lookup("$x"))));
)

COMPILE_FN(num, compiledfn_num, "($x)",
  return mkref(new_num(atof(to_string(lookup("$x")).c_str())));
)
