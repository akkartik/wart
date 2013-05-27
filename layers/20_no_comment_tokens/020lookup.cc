//// symbol bindings

unordered_map<Cell*, Cell*> bindings;

Cell* lookup(string s) {
  return lookup(newSym(s));
}

Cell* lookup(Cell* sym) {
  if (!bindings[sym]) {
    RAISE << "No binding for " << toString(sym) << endl;
    return nil;
  }
  return bindings[sym];
}

void newBinding(string sym, Cell* val) {
  newBinding(newSym(sym), val);
}

void newBinding(Cell* sym, Cell* val) {
  trace("bind") << sym << ": " << val << '\n';
  bindings[sym] = val;
}

void teardownBindings() {
  bindings.clear();
}
