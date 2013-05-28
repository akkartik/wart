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
  if (bindings[sym] == val) return;
  if (!bindings[sym]) mkref(sym);
  if (bindings[sym]) rmref(bindings[sym]);
  mkref(val);
  bindings[sym] = val;
}

void teardownBindings() {
  for (unordered_map<cell*, cell*>::iterator p = bindings.begin(); p != bindings.end(); ++p) {
    if (!p->second) continue;
    rmref(p->first);
    rmref(p->second);
  }
  bindings.clear();
}
