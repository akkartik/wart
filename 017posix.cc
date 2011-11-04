COMPILE_PRIM_FUNC(fork, primFunc_fork, L"()",
  return mkref(newNum(fork()));
)

COMPILE_PRIM_FUNC(wait_for_child, primFunc_wait_for_child, L"()",
  wait(NULL);
  return nil;
)
