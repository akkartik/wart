//// symbol bindings

unordered_map<cell*, cell*> bindings;

cell* lookup(string s) {
  return lookup(new_sym(s));
}

cell* lookup(cell* sym) {
  if (!bindings[sym]) {
    RAISE << "No binding for " << to_string(sym) << '\n';
    return nil;
  }
  return bindings[sym];
}

void new_binding(string sym, cell* val) {
  new_binding(new_sym(sym), val);
}

void new_binding(cell* sym, cell* val) {
  trace("bind") << sym << ": " << val;
  bindings[sym] = val;
}

void teardown_bindings() {
  bindings.clear();
}
