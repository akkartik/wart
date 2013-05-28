void add_cons(cell* p, cell* x) {
  set_cdr(p, new_cons(x));
}
