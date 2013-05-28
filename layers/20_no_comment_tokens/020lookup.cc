//// symbol bindings

unordered_map<cell*, cell*> bindings;

cell* lookup(string s) {
  return lookup(new_sym(s));
}

cell* lookup(cell* sym) {
  if (!bindings[sym]) {
    RAISE << "No binding for " << to_string(sym) << endl;
    return nil;
  }
  return bindings[sym];
}

void newBinding(string sym, cell* val) {
  newBinding(new_sym(sym), val);
}

void newBinding(cell* sym, cell* val) {
  trace("bind") << sym << ": " << val;
  bindings[sym] = val;
}

void teardownBindings() {
  bindings.clear();
}
