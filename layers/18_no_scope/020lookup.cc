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

void new_binding(string sym, cell* val) {
  new_binding(new_sym(sym), val);
}

void new_binding(cell* sym, cell* val) {
  if (bindings[sym] == val) return;
  if (!bindings[sym]) mkref(sym);
  if (bindings[sym]) rmref(bindings[sym]);
  mkref(val);
  bindings[sym] = val;
}

void teardown_bindings() {
  for (unordered_map<cell*, cell*>::iterator p = bindings.begin(); p != bindings.end(); ++p) {
    if (!p->second) continue;
    rmref(p->first);
    rmref(p->second);
  }
  bindings.clear();
}
