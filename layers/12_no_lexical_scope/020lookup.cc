//// symbol bindings

// Dynamic scopes are for rebinding global variables, and for undoing bindings.
unordered_map<Cell*, stack<Cell*> > dynamics;

Cell* lookup(string s) {
  return lookup(newSym(s));
}

Cell* lookup(Cell* sym) {
  stack<Cell*>& bindings = dynamics[sym];
  if (bindings.empty()) {
    RAISE << "No binding for " << toString(sym) << endl;
    return nil;
  }
  return bindings.top();
}

void newDynamicScope(Cell* sym, Cell* val) {
  mkref(sym);
  mkref(val);
  dynamics[sym].push(val);
}

void newDynamicScope(string s, Cell* val) {
  newDynamicScope(newSym(s), val);
}

void endDynamicScope(Cell* sym) {
  stack<Cell*>& bindings = dynamics[sym];
  if (bindings.empty()) {
    RAISE << "No dynamic binding for " << sym << endl;
    return;
  }
  rmref(sym);
  rmref(bindings.top());
  bindings.pop();
}

void endDynamicScope(string s) {
  endDynamicScope(newSym(s));
}

void assignDynamicVar(Cell* sym, Cell* val) {
  stack<Cell*>& bindings = dynamics[sym];
  if (bindings.empty()) {
    RAISE << "No dynamic binding to assign for " << sym << endl;
    newDynamicScope(sym, val);
    return;
  }
  rmref(bindings.top());
  bindings.pop();
  mkref(val);
  bindings.push(val);
}
