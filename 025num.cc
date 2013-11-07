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

#include <math.h>
COMPILE_FN(^, compiledfn_exp, "($base $exp)",
  return mkref(new_num(pow(to_float(lookup("$base")), to_float(lookup("$exp")))));
)

COMPILE_FN(<, compiledfn_lesser, "($x $y)",
  cell* x = lookup("$x");
  cell* y = lookup("$y");
  if (x == sym_false || y == sym_false) return mkref(sym_false);
  return to_float(x) < to_float(y) ? mkref(y) : mkref(sym_false);
)

COMPILE_FN(int, compiledfn_integer, "($x)",
  return mkref(new_num(to_int(lookup("$x"))));
)

COMPILE_FN(num, compiledfn_num, "($x)",
  return mkref(new_num(atof(to_string(lookup("$x")).c_str())));
)
