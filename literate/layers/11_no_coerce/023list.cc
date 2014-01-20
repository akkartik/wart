COMPILE_FN(cons, compiledfn_cons, "($x $y)",
  return mkref(new_cons(lookup("$x"), lookup("$y")));
)

// useful idiom: create a dummy cell p, keep appending to it using add_cons,
// then return drop_ptr(p) which GC's the dummy but mkrefs the rest.
cell* drop_ptr(cell* p) {
  cell* x = mkref(cdr(p));
  rmref(p);
  return x;
}

void add_cons(cell* p, cell* x) {
  set_cdr(p, new_cons(x));
}
