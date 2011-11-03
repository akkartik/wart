COMPILE_PRIM_FUNC(fork, primFunc_fork, L"()",
  return mkref(newNum(fork()));
)
