//// symbol bindings

// Dynamic scopes are for rebinding global variables, and for undoing bindings.
map<cell*, stack<cell*> > Dynamics;

// The current lexical scope is a first-class dynamic variable, usually bound
// to a table of bindings.
//
// Entering lexical scopes modifies the current dynamic binding of
// Curr_lexical_scope; calling functions creates new dynamic bindings
// to Curr_lexical_scope
cell* CURR_LEXICAL_SCOPE;
#define Curr_lexical_scopes  Dynamics[CURR_LEXICAL_SCOPE]
#define Curr_lexical_scope  Curr_lexical_scopes.top()

cell* lookup(string s) {
  return lookup(new_sym(s));
}

bool Warn_on_unknown_var = true;

// keep_already_evald is for supporting @args in macro calls
cell* lookup(cell* sym, cell* scope, bool keep_already_evald) {
  trace("already_evald") << "lookup " << sym << " " << keep_already_evald;
  cell* result = lookup_lexical_binding(sym, scope);
  if (result) return maybe_strip_already_evald(keep_already_evald, result);
  result = lookup_dynamic_binding(sym);
  if (result) return maybe_strip_already_evald(keep_already_evald, result);
  if (Warn_on_unknown_var) {
    RAISE << "no binding for " << sym << '\n';
    return nil;
  }
  trace("lookup") << "incomplete_eval";
  return new_object("incomplete_eval", sym);
}

cell* lookup(cell* sym) {
  return lookup(sym, Curr_lexical_scope);
}

cell* lookup(cell* sym, cell* scope) {
  return lookup(sym, scope, false);
}



//// dynamic scope

cell* lookup_dynamic_binding(cell* sym) {
  stack<cell*>& bindings = Dynamics[sym];
  if (bindings.empty()) return NULL;
  return bindings.top();
}

void new_dynamic_scope(cell* sym, cell* val) {
  trace("bind") << sym << ": " << val;
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
    RAISE << "No dynamic binding for " << sym << '\n';
    return;
  }
  trace("unbind") << sym;
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
    RAISE << "No dynamic binding to assign for " << sym << '\n';
    new_dynamic_scope(sym, val);
    return;
  }
  rmref(bindings.top());
  bindings.pop();
  mkref(val);
  bindings.push(val);
}



//// lexical scope -- a table, except that nils are not deleted

cell* lookup_lexical_binding(cell* sym, cell* scope) {
  cell* result = NULL;
  for (; scope != nil; scope = cdr(scope)) {
    if (is_table(scope))
      result = unsafe_get(scope, sym);
    if (result) return result;
  }

  return NULL;
}

cell* scope_containing_binding(cell* sym, cell* scope) {
  for (; scope != nil; scope = cdr(scope)) {
    if (is_table(scope) && unsafe_get(scope, sym))
      return scope;
  }

  if (lookup_dynamic_binding(sym))
    return nil;
  return NULL;
}

void new_lexical_scope() {
  cell* new_scope = new_table();
  set_cdr(new_scope, Curr_lexical_scope);
  new_dynamic_scope(CURR_LEXICAL_SCOPE, new_scope);
}

void end_lexical_scope() {
  if (Curr_lexical_scope == nil)
    RAISE << "No lexical scope to end\n";
  end_dynamic_scope(CURR_LEXICAL_SCOPE);
}

void add_lexical_scope(cell* new_scope) {
  set_cdr(new_scope, Curr_lexical_scope);
  new_dynamic_scope(CURR_LEXICAL_SCOPE, new_scope);
}

void add_lexical_binding(cell* sym, cell* val, cell* scope) {
  if (unsafe_get(scope, sym))
    RAISE << "Can't rebind within a lexical scope: " << sym << '\n';
  trace("bind") << sym << ": " << val;
  unsafe_put(scope, sym, val, false);  // deleting nil might expose a shadowed binding
}

void add_lexical_binding(cell* sym, cell* val) {
  add_lexical_binding(sym, val, Curr_lexical_scope);
}

void add_lexical_binding(string var, cell* val, cell* scope) {
  add_lexical_binding(new_sym(var), val, scope);
}

void add_lexical_binding(string var, cell* val) {
  add_lexical_binding(new_sym(var), val);
}



//// internals

void setup_scopes() {
  Dynamics.clear();  // leaks memory for strings and tables
  CURR_LEXICAL_SCOPE = new_sym("Curr_lexical_scope");
  new_dynamic_scope(CURR_LEXICAL_SCOPE, nil);
}
