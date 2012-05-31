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

void addLexicalBinding(Cell* sym, Cell* val, Cell* scope) {
  if (unsafeGet(scope, sym))
    RAISE << "Can't rebind within a lexical scope" << endl << DIE;
  unsafeSet(scope, sym, val, false); // deleting nil might expose a shadowed binding
}

void addLexicalBinding(Cell* sym, Cell* val) {
  addLexicalBinding(sym, val, currLexicalScopes.top());
}

void addLexicalBinding(string var, Cell* val, Cell* scope) {
  addLexicalBinding(newSym(var), val, scope);
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

                                  bool isAlreadyEvald(Cell*);

                                  bool containsAlreadyEvald(Cell* x) {
                                    if (!isCons(x)) return false;
                                    if (isAlreadyEvald(x)) return true;
                                    return containsAlreadyEvald(car(x))
                                           || containsAlreadyEvald(cdr(x));
                                  }

                                  bool skippedAlreadyEvald = false;
                                  Cell* maybeStripAlreadyEvald(bool dontReallyStrip, Cell* x) {
                                    if (dontReallyStrip) {
                                      skippedAlreadyEvald = containsAlreadyEvald(x);
                                      return x;
                                    }
                                    if (isAlreadyEvald(x))
                                      return cdr(x);
                                    return x;
                                  }

Cell* lookup(Cell* sym, Cell* scope, bool keepAlreadyEval) {
  Cell* result = lookupLexicalBinding(sym, scope);
  if (result) return maybeStripAlreadyEvald(keepAlreadyEval, result);
  result = lookupDynamicBinding(sym);
  if (result) return maybeStripAlreadyEvald(keepAlreadyEval, result);
  RAISE << "No binding for " << toString(sym) << endl;

  if (pretendRaise) return nil; // don't die
  cerr << "- Did you not want a symbol lookup? Perhaps the expression is indented too much." << endl;
  cerr << "- Was it defined using indentation? When wart encounters a paren in the middle of a line, it stops inserting parens until it closes." << endl << DIE;
  return nil;
}

Cell* lookup(Cell* sym, Cell* scope) {
  return lookup(sym, scope, false);
}

Cell* lookup(Cell* sym) {
  return lookup(sym, currLexicalScopes.top());
}

Cell* lookup(string s) {
  return lookup(newSym(s));
}
