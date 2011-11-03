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
    newDynamicScope(sym, val);
    return;
  }
  rmref(bindings.top());
  bindings.pop();
  mkref(val);
  bindings.push(val);
}



hash_set<Cell*, TypeCastCellHash> initialSyms;
void setupInitialSym(Cell* var, Cell* val) {
  newDynamicScope(var, val);
  initialSyms.insert(var);
}

// the current lexical scope is a first-class dynamic variable
Cell* CURR_LEXICAL_SCOPE;
#define currLexicalScopes dynamics[CURR_LEXICAL_SCOPE]
void setupLexicalScope() {
  CURR_LEXICAL_SCOPE = newSym(L"currLexicalScope");
  setupInitialSym(CURR_LEXICAL_SCOPE, nil);
}

// entering and leaving lexical scopes *assigns the current dynamic*
// binding of the currLexicalScope sym.
// Calling functions will create new dynamic bindings.
void newLexicalScope() {
  Cell* newScope = newTable();
  dbg << "new lexical scope: " << (void*)newScope << endl;
  setCdr(newScope, currLexicalScopes.top());
  newDynamicScope(CURR_LEXICAL_SCOPE, newScope);
}

void endLexicalScope() {
  Cell* currScope = currLexicalScopes.top();
  if (currScope == nil)
    err << "No lexical scope to end" << endl << DIE;
  dbg << "end lexical scope: " << (void*)currScope << endl;
  endDynamicScope(CURR_LEXICAL_SCOPE);
}

void addLexicalBinding(Cell* sym, Cell* val) {
  dbg << "creating binding: " << (void*)currLexicalScopes.top() << " " << (void*)sym << endl;
  if (unsafeGet(currLexicalScopes.top(), sym))
    err << "Can't rebind within a lexical scope" << endl << DIE;
  unsafeSet(currLexicalScopes.top(), sym, val, false);
}
void addLexicalBinding(string var, Cell* val) {
  addLexicalBinding(newSym(var), val);
}



Cell* lookupLexicalBinding(Cell* sym, Cell* scope) {
  Cell* result = NULL;
  for (; scope != nil; scope = cdr(scope)) {
    if (isTable(scope))
      result = unsafeGet(scope, sym);
    if (result) return result;
  }

  return NULL;
}

Cell* scopeContainingBinding(Cell* sym, Cell* scope) {
  for (; scope != nil; scope = cdr(scope)) {
    if (isTable(scope) && unsafeGet(scope, sym))
      return scope;
  }

  if (lookupDynamicBinding(sym))
    return nil;
  return NULL;
}

Cell* lookup(Cell* sym) {
  Cell* result = lookupLexicalBinding(sym, currLexicalScopes.top());
  if (result) return result;
  result = lookupDynamicBinding(sym);
  if (result) return result;
  warn << "No binding for " << toString(sym)
    << " (if a paren opens in the middle of a line, paren-insertion is disabled until it closes)" << endl;
  return nil;
}

Cell* lookup(string s) {
  return lookup(newSym(s));
}
