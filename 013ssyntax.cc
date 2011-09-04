struct SsyntaxTemplate {
  char key;
  enum { BEGINNING, IN_BETWEEN, END } dir;
  Cell* convertedSym;
};
list<SsyntaxTemplate> ssyntaxTemplates;

string transformSsyntax(string var, SsyntaxTemplate pat) {
  size_t pos = var.find(pat.key);
  if (pos == string::npos) return L"";

  // avoid detecting 2.4 as ssyntax. Hacky, not unicode-aware.
  if (var.find_first_of(L"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") == string::npos) return L"";

  size_t len = var.length();
  if (pos == 0) {
    if (pat.dir != SsyntaxTemplate::BEGINNING) return L"";
    return var.substr(1, len);
  }

  if (pos == len-1) {
    if (pat.dir != SsyntaxTemplate::END) return L"";
    return var.substr(0, len-1);
  }

  while (pos != string::npos) {
    var.replace(pos, 1, 1, L' ');
    pos = var.find(pat.key, pos);
  }
  return var;
}

Cell* transform_ssyntax(Cell* input) {
  if (isSym(input)) {
    string var = toString(input);
    for (list<SsyntaxTemplate>::iterator p = ssyntaxTemplates.begin(); p != ssyntaxTemplates.end(); ++p) {
      string rest = transformSsyntax(var, *p);
      if (!rest.empty()) {
        Cell* args = wartRead(stream(rest)).front();
        if (!isCons(args)) args = newCons(args, nil); // unary op
        input = newCons(p->convertedSym, args);
        break;
      }
    }
  }

  if (!isCons(input)) return input; // no tables or primFuncs in static code
  setCar(input, transform_ssyntax(car(input)));
  setCdr(input, transform_ssyntax(cdr(input)));
  return input;
}

COMPILE_PRIM_FUNC(ssyntax, primFunc_ssyntax,
  string pat = toString(car(args));
  size_t pos = pat.find_first_not_of(L"_");
  if (pos == string::npos) return nil;

  SsyntaxTemplate s;
  s.key = pat[pos];
  if (pos == 0) s.dir = SsyntaxTemplate::BEGINNING;
  else if (pos == pat.length()) s.dir = SsyntaxTemplate::END;
  else s.dir = SsyntaxTemplate::IN_BETWEEN;
  s.convertedSym = car(car(cdr(args)));
  ssyntaxTemplates.push_back(s);
  return nil;
)
