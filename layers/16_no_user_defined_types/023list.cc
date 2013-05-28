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

// push p onto l and move one refcount to new head
cell* push_cons(cell* p, cell* l) {
  cell* result = new_cons(p, l);
  rmref(cdr(result));
  return mkref(result);
}
