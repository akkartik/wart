//// symbol bindings

// Dynamic scopes are for rebinding global variables, and for undoing bindings.
unordered_map<Cell*, stack<Cell*> > dynamics;

// The current lexical scope is a first-class dynamic variable, usually bound
// to a table of bindings.
//
// Entering lexical scopes modifies the current dynamic binding of
// currLexicalScope; calling functions creates new dynamic bindings
// to currLexicalScope
Cell* CURR_LEXICAL_SCOPE;
#define currLexicalScopes dynamics[CURR_LEXICAL_SCOPE]
#define currLexicalScope currLexicalScopes.top()

Cell* lookup(string s) {
  return lookup(newSym(s));
}

// keepAlreadyEvald is for supporting @args in macro calls
Cell* lookup(Cell* sym, Cell* scope, bool keepAlreadyEvald) {
  Cell* result = lookupLexicalBinding(sym, scope);
  if (result) return maybeStripAlreadyEvald(keepAlreadyEvald, result);
  result = lookupDynamicBinding(sym);
  if (result) return maybeStripAlreadyEvald(keepAlreadyEvald, result);
  RAISE << "No binding for " << toString(sym) << endl;
  if (!pretendRaise)
    cerr << "- Was it defined using indentation? Wart ignores indentation inside parens." << endl << DIE;
  return nil;
}

Cell* lookup(Cell* sym) {
  return lookup(sym, currLexicalScope);
}

Cell* lookup(Cell* sym, Cell* scope) {
  return lookup(sym, scope, false);
}



// dynamic scope

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



// lexical scope

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

void newLexicalScope() {
  Cell* newScope = newTable();
  setCdr(newScope, currLexicalScope);
  newDynamicScope(CURR_LEXICAL_SCOPE, newScope);
}

void endLexicalScope() {
  Cell* currScope = currLexicalScope;
  if (currScope == nil)
    RAISE << "No lexical scope to end" << endl << DIE;
  endDynamicScope(CURR_LEXICAL_SCOPE);
}

void addLexicalBinding(Cell* sym, Cell* val, Cell* scope) {
  if (unsafeGet(scope, sym))
    RAISE << "Can't rebind within a lexical scope" << endl << DIE;
  unsafeSet(scope, sym, val, false);  // deleting nil might expose a shadowed binding
}

void addLexicalBinding(Cell* sym, Cell* val) {
  addLexicalBinding(sym, val, currLexicalScope);
}

void addLexicalBinding(string var, Cell* val, Cell* scope) {
  addLexicalBinding(newSym(var), val, scope);
}

void addLexicalBinding(string var, Cell* val) {
  addLexicalBinding(newSym(var), val);
}



// misc

unordered_set<Cell*> initialSyms;

void setupScopes() {
  dynamics.clear();   // leaks memory for strings and tables
  CURR_LEXICAL_SCOPE = newSym("currLexicalScope");
  newDynamicScope(CURR_LEXICAL_SCOPE, nil);
  initialSyms.insert(CURR_LEXICAL_SCOPE);
}
