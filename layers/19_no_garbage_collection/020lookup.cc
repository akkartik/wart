//// symbol bindings

unordered_map<cell*, cell*> Bindings;

cell* lookup(string s) {
  return lookup(new_sym(s));
}

cell* lookup(cell* sym) {
  if (!Bindings[sym]) {
    RAISE << "No binding for " << to_string(sym) << '\n';
    return nil;
  }
  return Bindings[sym];
}

void new_binding(string sym, cell* val) {
  new_binding(new_sym(sym), val);
}

void new_binding(cell* sym, cell* val) {
  Bindings[sym] = val;
}

void teardown_bindings() {
  Bindings.clear();
}
