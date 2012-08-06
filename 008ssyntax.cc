//// simple syntax shorthands transforming syms into forms

Cell* transformNot(string var) {
  var.replace(0, 1, "not ");
  return nextRawCell(CodeStream(stream(var)));
}

Cell* transformCompose(string var) {
  var.replace(var.rfind(':'), 1, " ");
  return nextRawCell(stream("compose "+var));
}

Cell* transformCall(string var) {
  size_t end = var.length()-1;
  if (var.rfind('.') == end)
    return newCons(newSym(var.substr(0, end)));

  size_t dot = var.rfind('.');
  size_t bang = end > 0 ? var.rfind('!', end-1) : var.rfind('!');
  if (bang != string::npos && (dot == string::npos || bang > dot))
    var.replace(bang, 1, " '");
  else
    var.replace(dot, 1, " ");
  return nextRawCell(stream(var));
}

Cell* transformAndf(string var) {
  var.replace(var.rfind('&'), 1, " ");
  return nextRawCell(stream("andf "+var));
}

Cell* transformComplement(string var) {
  var.replace(0, 1, "complement ");
  return nextRawCell(stream(var));
}

Cell* transform_ssyntax(Cell* input) {
  if (isSym(input)) {
    string var = toString(input);
    // avoid detecting floats as ssyntax
    if (var.find_first_not_of("0123456789.:!~&") == string::npos)
      ;
    else if (var[0] == '!')
      input = transformNot(var);
    else if (var.find('.') != string::npos || var.find('!') < var.length()-1)
      input = transformCall(var);
    else if (var[0] != ':' && var.find(':') != string::npos)
      input = transformCompose(var);
    else if (var.find('&') != string::npos)
      input = transformAndf(var);
    else if (var[0] == '~')
      input = transformComplement(var);
  }

  if (!isCons(input)) return input; // no tables or compiledFns in static code
  setCar(input, transform_ssyntax(car(input)));
  setCdr(input, transform_ssyntax(cdr(input)));
  return input;
}
