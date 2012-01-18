//// manage symbol bindings

unordered_map<Cell*, stack<Cell*> > dynamics;
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



// the current lexical scope is a first-class dynamic variable
Cell* CURR_LEXICAL_SCOPE;
#define currLexicalScopes dynamics[CURR_LEXICAL_SCOPE]
void setupLexicalScope() {
  CURR_LEXICAL_SCOPE = newSym("currLexicalScope");
  newDynamicScope(CURR_LEXICAL_SCOPE, nil);
  initialSyms.insert(CURR_LEXICAL_SCOPE);
}

// entering and leaving lexical scopes *assigns the current dynamic*
// binding of the currLexicalScope sym.
// Calling functions will create new dynamic bindings.
void newLexicalScope() {
  Cell* newScope = newTable();
  setCdr(newScope, currLexicalScopes.top());
  newDynamicScope(CURR_LEXICAL_SCOPE, newScope);
}

void endLexicalScope() {
  Cell* currScope = currLexicalScopes.top();
  if (currScope == nil)
    RAISE << "No lexical scope to end" << endl << DIE;
  endDynamicScope(CURR_LEXICAL_SCOPE);
}

void addLexicalBinding(Cell* sym, Cell* val) {
  if (unsafeGet(currLexicalScopes.top(), sym))
    RAISE << "Can't rebind within a lexical scope" << endl << DIE;
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
  RAISE << "No binding for " << toString(sym) << endl;

  if (inTest) return nil; // don't die
  cerr << "- Did you not want a symbol lookup? Perhaps the expression is indented too much." << endl;
  cerr << "- Was it defined using indentation? When wart encounters a paren in the middle of a line, it stops inserting parens until it closes." << endl << DIE;
  return nil;
}

Cell* lookup(string s) {
  return lookup(newSym(s));
}
