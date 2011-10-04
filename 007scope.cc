//// manage symbol bindings

hash_map<Cell*, stack<Cell*>, TypeCastCellHash> dynamics;
Cell* lookupDynamicBinding(Cell* sym) {
  stack<Cell*>& bindings = dynamics[sym];
  if (bindings.empty()) return NULL;
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
    warn << "No dynamic binding for " << sym << endl;
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
    warn << "No dynamic binding to assign for " << sym << endl;
    return;
  }
  rmref(bindings.top());
  bindings.pop();
  mkref(val);
  bindings.push(val);
}



// the current lexical scope is a first-class dynamic variable
#define currLexicalScopes dynamics[newSym(L"currLexicalScope")]
hash_set<Cell*, TypeCastCellHash> initialSyms;
void setupLexicalScope() {
  newDynamicScope(L"currLexicalScope", newSym(L"dynamicScope"));
  initialSyms.insert(newSym(L"currLexicalScope"));
  initialSyms.insert(newSym(L"dynamicScope"));
}

// entering and leaving lexical scopes *assigns the current dynamic*
// binding of the currLexicalScope sym.
// Calling functions will create new dynamic bindings.
void newLexicalScope() {
  Cell* newScope = newTable();
  dbg << "new lexical scope: " << newScope << endl;
  setCdr(newScope, currLexicalScopes.top());
  newDynamicScope(newSym(L"currLexicalScope"), newScope);
}

void endLexicalScope() {
  Cell* currScope = currLexicalScopes.top();
  if (currScope == nil)
    err << "No lexical scope to end" << endl << DIE;
  dbg << "end lexical scope: " << currScope << endl;
  endDynamicScope(newSym(L"currLexicalScope"));
}

void addLexicalBinding(Cell* sym, Cell* val) {
  dbg << "creating binding: " << (void*)currLexicalScopes.top() << " " << sym << endl;
  if (unsafeGet(currLexicalScopes.top(), sym)) err << "Can't rebind within a lexical scope" << endl << DIE;
  unsafeSet(currLexicalScopes.top(), sym, val, false);
}
void addLexicalBinding(string var, Cell* val) {
  addLexicalBinding(newSym(var), val);
}



Cell* lookupLexicalBinding(Cell* sym, Cell* scope) {
  Cell* result = NULL;
  list<Cell*> callees;
  for (; scope != nil; scope = cdr(scope)) {
    if (scope == newSym(L"dynamicScope"))
      result = lookupDynamicBinding(sym);
    else if (isTable(scope))
      result = unsafeGet(scope, sym);
    if (result) return result;

    callees.push_back(scope);
  }

  for (list<Cell*>::iterator p = callees.begin(); p != callees.end(); ++p) {
    if (!isCons(*p)) continue;
    result = lookupLexicalBinding(sym, car(*p));
    if (result) return result;
  }
  return NULL;
}

Cell* scopeContainingBinding(Cell* sym, Cell* scope) {
  list<Cell*> callees;
  for (; scope != nil; scope = cdr(scope)) {
    if (scope == newSym(L"dynamicScope") && lookupDynamicBinding(sym))
      return scope;
    if (isTable(scope) && unsafeGet(scope, sym))
      return scope;

    if (isCons(scope))
      callees.push_back(scope);
  }

  for (list<Cell*>::iterator p = callees.begin(); p != callees.end(); ++p) {
    Cell* result = scopeContainingBinding(sym, car(*p));
    if (result) return result;
  }
  return NULL;
}

Cell* lookup(Cell* sym) {
  Cell* result = lookupLexicalBinding(sym, currLexicalScopes.top());
  if (result) return result;
  warn << "No binding for " << toString(sym) << endl;
  return nil;
}

Cell* lookup(string s) {
  return lookup(newSym(s));
}
