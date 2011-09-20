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
  stack<Cell*>& bindings = dynamics[(long)sym];
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
#define currLexicalScopes dynamics[(long)newSym(L"currLexicalScope")]
hash_set<Cell*, TypeCastCellHash> initialSyms;
void setupLexicalScope() {
  newDynamicScope(L"currLexicalScope", newSym(L"dynamicScope"));
  initialSyms.insert(newSym(L"currLexicalScope"));
  initialSyms.insert(newSym(L"dynamicScope"));
}

Cell* lookupLexicalBinding(Cell* sym, Cell* lexicalScope) {
  for (Cell* scope = lexicalScope; scope != nil; scope = cdr(scope)) {
    Cell* result = NULL;
    if (scope == newSym(L"dynamicScope"))
      result = lookupDynamicBinding(sym);
    else if (isTable(scope))
      result = unsafeGet(scope, sym);
    else if (isCons(scope))
      result = lookupLexicalBinding(sym, car(scope));
    if (result) return result;
  }
  return NULL;
}

Cell* scopeContainingBinding(Cell* sym, Cell* lexicalScope) {
  for (Cell* scope = lexicalScope; scope != nil; scope = cdr(scope)) {
    if (scope == newSym(L"dynamicScope") && lookupDynamicBinding(sym))
      return scope;
    else if (isTable(scope) && unsafeGet(scope, sym))
      return scope;
    else if (isCons(scope)) {
      Cell* result = scopeContainingBinding(sym, car(scope));
      if (result) return result;
    }
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

Cell* lookup(Cell* sym) {
  Cell* result = lookupLexicalBinding(sym, currLexicalScopes.top());
  if (result) return result;
  warn << "No binding for " << toString(sym) << endl;
  return nil;
}

Cell* lookup(string s) {
  return lookup(newSym(s));
}
