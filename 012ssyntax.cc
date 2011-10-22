//// simple syntax shorthands transforming syms into forms

Cell* transformNot(string var) {
  var.replace(0, 1, L"not ");
  return buildFromStream(stream(var)).front();
}

Cell* transformCompose(string var) {
  var.replace(var.rfind(L':'), 1, L" ");
  return buildFromStream(stream(L"compose "+var)).front();
}

Cell* transformCall(string var) {
  int end = var.length()-1;
  if (var.rfind(L'.') == end)
    return newCons(newSym(var.substr(0, end)), nil);

  size_t dot = var.rfind(L'.');
  size_t bang = end > 0 ? var.rfind(L'!', end-1) : var.rfind(L'!');
  if (bang != string::npos && (dot == string::npos || bang > dot))
    var.replace(bang, 1, L" '");
  else
    var.replace(dot, 1, L" ");
  return buildFromStream(stream(var)).front();
}

Cell* transformAndf(string var) {
  var.replace(var.rfind(L'&'), 1, L" ");
  return buildFromStream(stream(L"andf "+var)).front();
}

Cell* transformComplement(string var) {
  var.replace(0, 1, L"complement ");
  return buildFromStream(stream(var)).front();
}

Cell* transform_ssyntax(Cell* input) {
  if (isSym(input)) {
    string var = toString(input);
    // avoid detecting floats as ssyntax. Hacky, not unicode-aware.
    if (var.find_first_of(L"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") == string::npos)
      ;
    else if (var[0] == L'!')
      input = transformNot(var);
    else if (var[0] != L':' && var.find(L':') != string::npos)
      input = transformCompose(var);
    else if (var.find(L'.') != string::npos || var.find(L'!') < var.length()-1)
      input = transformCall(var);
    else if (var.find(L'&') != string::npos)
      input = transformAndf(var);
    else if (var[0] == L'~')
      input = transformComplement(var);
  }

  if (!isCons(input)) return input; // no tables or primFuncs in static code
  setCar(input, transform_ssyntax(car(input)));
  setCdr(input, transform_ssyntax(cdr(input)));
  return input;
}
