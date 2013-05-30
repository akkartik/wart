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
  trace("bind") << sym << ": " << val;
  if (Bindings[sym] == val) return;
  if (!Bindings[sym]) mkref(sym);
  if (Bindings[sym]) rmref(Bindings[sym]);
  Bindings[sym] = mkref(val);
}

void teardown_bindings() {
  for (unordered_map<cell*, cell*>::iterator p = Bindings.begin(); p != Bindings.end(); ++p) {
    if (!p->second) continue;
    rmref(p->first);
    rmref(p->second);
  }
  Bindings.clear();
}
