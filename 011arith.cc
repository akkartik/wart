COMPILE_PRIM_FUNC(+, primFunc_add,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = newNum(toNum(x) + toNum(y));
  rmref(x);
  rmref(y);
  return mkref(result);
)

COMPILE_PRIM_FUNC(-, primFunc_subtract,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = newNum(toNum(x) - toNum(y));
  rmref(x);
  rmref(y);
  return mkref(result);
)

COMPILE_PRIM_FUNC(*, primFunc_multiply,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = newNum(toNum(x) * toNum(y));
  rmref(x);
  rmref(y);
  return mkref(result);
)

COMPILE_PRIM_FUNC(/, primFunc_divide,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = newNum(toNum(x) / toNum(y));
  rmref(x);
  rmref(y);
  return mkref(result);
)

COMPILE_PRIM_FUNC(%, primFunc_modulo,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = newNum(toNum(x) % toNum(y));
  rmref(x);
  rmref(y);
  return mkref(result);
)

COMPILE_PRIM_FUNC(>, primFunc_greater,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = (toNum(x) > toNum(y)) ? newNum(1) : nil;
  rmref(x);
  rmref(y);
  return mkref(result);
)
