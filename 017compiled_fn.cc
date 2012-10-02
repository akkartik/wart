//// compiled primitives

typedef Cell* (*CompiledFn)();

#define COMPILE_FN(op, name, params, body) \
  Cell* name() { body }   // op and params extracted into compiled_fn_list below

struct CompiledFnMetadata {
  string name;
  string params;
  CompiledFn impl;
};

const CompiledFnMetadata compiledFns[] = {
  #include "compiled_fn_list"
};

void setupCompiledFns() {
  newDynamicScope("compiled", newTable());
  for (unsigned long i=0; i < sizeof(compiledFns)/sizeof(compiledFns[0]); ++i) {
    Cell* f = newTable();
    set(f, newSym("name"), newSym(compiledFns[i].name));
    set(f, newSym("sig"), nextRawCell(stream(compiledFns[i].params)));
    set(f, newSym("body"), newCompiledFn(compiledFns[i].impl));
    Cell* obj = newObject("function", f);
    newDynamicScope(compiledFns[i].name, obj);
    // save to a second, immutable place
    set(lookup("compiled"), compiledFns[i].name, obj);
  }
}

void teardownCompiledFns() {
  for (unsigned long i=0; i < sizeof(compiledFns)/sizeof(compiledFns[0]); ++i)
    endDynamicScope(compiledFns[i].name);
  endDynamicScope("compiled");
}

Cell* newCompiledFn(CompiledFn f) {
  Cell* result = newCell();
  result->type = COMPILED_FN;
  result->car = (Cell*)f;
  return result;
}

bool isCompiledFn(Cell* x) {
  return x->type == COMPILED_FN;
}

CompiledFn toCompiledFn(Cell* x) {
  if (!isCompiledFn(x))
    RAISE << "Not a compiled function" << endl << DIE;
  return (CompiledFn)x->car;
}
