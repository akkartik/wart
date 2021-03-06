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
  if (lookup("$x") == nil || lookup("$y") == nil)
    return nil;
  return to_float(lookup("$x")) < to_float(lookup("$y")) ? mkref(lookup("$y")) : nil;
)

COMPILE_FN(int, compiledfn_integer, "($x)",
  return mkref(new_num(to_int(lookup("$x"))));
)
