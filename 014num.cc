COMPILE_FN(+, compiledFn_add, "($x $y)",
  Cell* x = lookup("$x"); Cell* y = lookup("$y");
  if (x->type == FLOAT || y->type == FLOAT)
    return mkref(newNum(toFloat(x) + toFloat(y)));
  else
    return mkref(newNum(toInt(x) + toInt(y)));
)

COMPILE_FN(-, compiledFn_subtract, "($x $y)",
  Cell* x = lookup("$x"); Cell* y = lookup("$y");
  if (x->type == FLOAT || y->type == FLOAT)
    return mkref(newNum(toFloat(x) - toFloat(y)));
  else
    return mkref(newNum(toInt(x) - toInt(y)));
)

COMPILE_FN(*, compiledFn_multiply, "($x $y)",
  Cell* x = lookup("$x"); Cell* y = lookup("$y");
  if (x->type == FLOAT || y->type == FLOAT)
    return mkref(newNum(toFloat(x) * toFloat(y)));
  else
    return mkref(newNum(toInt(x) * toInt(y)));
)

COMPILE_FN(/, compiledFn_divide, "($x $y)",
  Cell* x = lookup("$x"); Cell* y = lookup("$y");
  return mkref(newNum(toFloat(x) / toFloat(y)));
)

COMPILE_FN(%, compiledFn_modulo, "($x $y)",
  return mkref(newNum(toInt(lookup("$x")) % toInt(lookup("$y")))); // what does modulo of floats mean?
)

COMPILE_FN(<, compiledFn_lesser, "($x $y)",
  return toFloat(lookup("$x")) < toFloat(lookup("$y")) ? mkref(lookup("$x")) : nil;
)

COMPILE_FN(int, compiledFn_integer, "($x)",
  return mkref(newNum(toInt(lookup("$x"))));
)
