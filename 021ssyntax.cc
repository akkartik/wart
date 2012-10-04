//// simple syntax shorthands transforming syms into forms

Cell* transform_ssyntax(Cell* x) {
  if (isSym(x)) {
    string var = toString(x);
    // avoid detecting floats as ssyntax
    if (var.find_first_not_of("0123456789."+ssyntaxChars) == NOT_FOUND)
      ;
    else if (var[0] == '!')
      x = expandNot(var);
    else if (find(var, '.') || var.find('!') < var.length()-1)
      x = expandCall(var);
  }

  if (!isCons(x)) return x;   // no tables or compiledFns in static code
  setCar(x, transform_ssyntax(car(x)));
  setCdr(x, transform_ssyntax(cdr(x)));
  return x;
}



// internals

Cell* expandNot(string var) {
  var.replace(0, 1, "not ");
  return nextRawCell(stream(var));
}

Cell* expandCall(string var) {
  size_t end = var.length()-1;
  if (var.rfind('.') == end)
    return nextRawCell(stream("("+var.substr(0, end)+")"));

  size_t dot = var.rfind('.');
  size_t bang = end > 0 ? var.rfind('!', end-1) : var.rfind('!');
  if (bang != NOT_FOUND && (dot == NOT_FOUND || bang > dot))
    var.replace(bang, 1, " '");
  else
    var.replace(dot, 1, " ");
  return nextRawCell(stream(var));
}
