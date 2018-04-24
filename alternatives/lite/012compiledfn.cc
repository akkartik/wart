//// compiled primitives

typedef cell* (*compiledfn)();

#define COMPILE_FN(op, name, params, body) \
  cell* name() { body }  // op and params extracted into compiledfn_list below

struct compiledfn_metadata {
  string name;
  string params;
  compiledfn impl;
};

const compiledfn_metadata Compiledfns[] = {
  #include "compiledfn_list"  // auto-generated; see makefile
};

void setup_compiledfns() {
  new_dynamic_scope(sym_compiled, new_table());
  for (unsigned long i=0; i < sizeof(Compiledfns)/sizeof(Compiledfns[0]); ++i) {
    TEMP(f, mkref(new_table()));
    put(f, sym_name, new_sym(Compiledfns[i].name));
    indent_sensitive_stream ss(Compiledfns[i].params);
    put(f, sym_sig, next_cell(ss));
    put(f, sym_body, new_compiledfn(Compiledfns[i].impl));
    TEMP(obj, mkref(new_object("function", f)));
    new_dynamic_scope(Compiledfns[i].name, obj);
    // save to a second, immutable place
    put(lookup(sym_compiled), Compiledfns[i].name, obj);
  }
}

void teardown_compiledfns() {
  for (unsigned long i=0; i < sizeof(Compiledfns)/sizeof(Compiledfns[0]); ++i)
    end_dynamic_scope(Compiledfns[i].name);
  end_dynamic_scope(sym_compiled);
}

cell* new_compiledfn(compiledfn f) {
  cell* result = new_cell();
  result->type = COMPILED_FN;
  result->car = (cell*)f;
  return result;
}

bool is_compiledfn(cell* x) {
  return x->type == COMPILED_FN;
}

compiledfn to_compiledfn(cell* x) {
  if (!is_compiledfn(x))
    RAISE << "Not a compiled function\n" << die();
  return (compiledfn)x->car;
}
