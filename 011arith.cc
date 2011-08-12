COMPILE_PRIM_FUNC(add, primFunc_add,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = newNum(toNum(x) + toNum(y));
  rmref(x);
  rmref(y);
  return mkref(result);
)

COMPILE_PRIM_FUNC(subtract, primFunc_subtract,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = newNum(toNum(x) - toNum(y));
  rmref(x);
  rmref(y);
  return mkref(result);
)

COMPILE_PRIM_FUNC(multiply, primFunc_multiply,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = newNum(toNum(x) * toNum(y));
  rmref(x);
  rmref(y);
  return mkref(result);
)

COMPILE_PRIM_FUNC(divide, primFunc_divide,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = newNum(toNum(x) / toNum(y));
  rmref(x);
  rmref(y);
  return mkref(result);
)

COMPILE_PRIM_FUNC(modulo, primFunc_modulo,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = newNum(toNum(x) % toNum(y));
  rmref(x);
  rmref(y);
  return mkref(result);
)

COMPILE_PRIM_FUNC(greater, primFunc_greater,
  Cell* x = eval(car(args));
  Cell* y = eval(car(cdr(args)));
  Cell* result = (toNum(x) > toNum(y)) ? newNum(1) : nil;
  rmref(x);
  rmref(y);
  return mkref(result);
)
