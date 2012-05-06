COMPILE_FN(+, compiledFn_add, "($x $y)",
  return mkref(newNum(toInt(lookup("$x")) + toInt(lookup("$y"))));
)

COMPILE_FN(-, compiledFn_subtract, "($x $y)",
  return mkref(newNum(toInt(lookup("$x")) - toInt(lookup("$y"))));
)

COMPILE_FN(*, compiledFn_multiply, "($x $y)",
  return mkref(newNum(toInt(lookup("$x")) * toInt(lookup("$y"))));
)

COMPILE_FN(/, compiledFn_divide, "($x $y)",
  return mkref(newNum(toInt(lookup("$x")) / toInt(lookup("$y"))));
)

COMPILE_FN(%, compiledFn_modulo, "($x $y)",
  return mkref(newNum(toInt(lookup("$x")) % toInt(lookup("$y"))));
)

COMPILE_FN(<, compiledFn_lesser, "($x $y)",
  return toInt(lookup("$x")) < toInt(lookup("$y")) ? mkref(lookup("$x")) : nil;
)
