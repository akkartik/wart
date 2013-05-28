//// symbol bindings

// Dynamic scopes are for rebinding global variables, and for undoing bindings.
unordered_map<cell*, stack<cell*> > Dynamics;

cell* lookup(string s) {
  return lookup(new_sym(s));
}

cell* lookup(cell* sym) {
  stack<cell*>& bindings = Dynamics[sym];
  if (bindings.empty()) {
    RAISE << "No binding for " << to_string(sym) << endl;
    return nil;
  }
  return bindings.top();
}

void new_dynamic_scope(cell* sym, cell* val) {
  mkref(sym);
  mkref(val);
  Dynamics[sym].push(val);
}

void new_dynamic_scope(string s, cell* val) {
  new_dynamic_scope(new_sym(s), val);
}

void end_dynamic_scope(cell* sym) {
  stack<cell*>& bindings = Dynamics[sym];
  if (bindings.empty()) {
    RAISE << "No dynamic binding for " << sym << endl;
    return;
  }
  rmref(sym);
  rmref(bindings.top());
  bindings.pop();
}

void end_dynamic_scope(string s) {
  end_dynamic_scope(new_sym(s));
}

void assign_dynamic_var(cell* sym, cell* val) {
  stack<cell*>& bindings = Dynamics[sym];
  if (bindings.empty()) {
    RAISE << "No dynamic binding to assign for " << sym << endl;
    new_dynamic_scope(sym, val);
    return;
  }
  rmref(bindings.top());
  bindings.pop();
  mkref(val);
  bindings.push(val);
}
