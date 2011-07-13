//// manage symbol bindings

hash_map<long, stack<Cell*> > dynamics;
Cell* lookupDynamicBinding(Cell* sym) {
  stack<Cell*> bindings = dynamics[(long)sym];
  if (bindings.empty()) return NULL;
  return bindings.top();
}

void newDynamicScope(Cell* sym, Cell* val) {
  mkref(sym);
  mkref(val);
  dynamics[(long)sym].push(val);
}

void newDynamicScope(string s, Cell* val) {
  newDynamicScope(newSym(s), val);
}

void endDynamicScope(Cell* sym) {
  stack<Cell*>& bindings = dynamics[(long)sym];
  if (bindings.empty()) {
    cerr << "No dynamic binding for " << sym << endl;
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
  stack<Cell*>& bindings = dynamics[(long)sym];
  if (bindings.empty()) {
    cerr << "No dynamic binding to assign for " << sym << endl;
    return;
  }
  rmref(bindings.top());
  bindings.pop();
  mkref(val);
  bindings.push(val);
}

// the current lexical scope is a first-class dynamic variable
#define currLexicalScopes dynamics[(long)newSym(L"currLexicalScope")]
void setupLexicalScope() {
  newDynamicScope(L"currLexicalScope", nil);
}

Cell* lookupLexicalBinding(Cell* sym) {
  for (Cell* scope = currLexicalScopes.top(); scope != nil; scope = cdr(scope)) {
    Cell* result = unsafeGet(scope, sym);
    if (result) return result;
  }
  return NULL;
}

// entering and leaving lexical scopes *assigns the current dynamic*
// binding of the currLexicalScope sym.
// Calling functions will create new dynamic bindings.
void newLexicalScope() {
  Cell* newScope = newTable();
  dbg << "new lexical scope: " << newScope << endl;
  setCdr(newScope, currLexicalScopes.top());
  mkref(newScope);
  assignDynamicVar(newSym(L"currLexicalScope"), newScope);
}

void endLexicalScope() {
  Cell* currScope = currLexicalScopes.top();
  if (currScope == nil)
    cerr << "No lexical scope to end" << endl << DIE;
  dbg << "end lexical scope: " << currScope << endl;
  Cell* oldScope = cdr(currScope);
  rmref(currScope);
  assignDynamicVar(newSym(L"currLexicalScope"), oldScope);
}

void addLexicalBinding(Cell* sym, Cell* val) {
  dbg << "creating binding: " << (void*)currLexicalScopes.top() << " " << sym << endl;
  if (unsafeGet(currLexicalScopes.top(), sym)) cerr << "Can't rebind within a lexical scope" << endl << DIE;
  unsafeSet(currLexicalScopes.top(), sym, val, false);
}

Cell* lookup(Cell* sym) {
  Cell* result = lookupLexicalBinding(sym);
  if (result) return result;
  result = lookupDynamicBinding(sym);
  if (result) return result;
  cerr << "No binding for " << toString(sym) << endl;
  return nil;
}

Cell* lookup(string s) {
  return lookup(newSym(s));
}
