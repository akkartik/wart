struct SsyntaxTemplate {
  char key;
  enum { BEGINNING, IN_BETWEEN, END } dir;
  Cell* convertedSym;
};
list<SsyntaxTemplate> ssyntaxTemplates;

string transformSsyntax(string var, SsyntaxTemplate pat) {
  size_t pos = var.find(pat.key);
  if (pos == string::npos) return L"";

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
      string args = transformSsyntax(var, *p);
      if (!args.empty()) {
        input = newCons(p->convertedSym, wartRead(stream(args)).front());
        break;
      }
    }
  }

  if (!isCons(input)) return input; // no tables or primFuncs in static code
  setCar(input, transform_ssyntax(car(input)));
  setCdr(input, transform_ssyntax(cdr(input)));
  return input;
}

// ssyntax !_ (not _)
// ssyntax ~_ (negate _)
// ssyntax _:_ (compose _ _)
// ssyntax _._ (call _ _)
// ssyntax _. (call _)
// ssyntax _!_ (call _ '_)

//? COMPILE_PRIM_FUNC(ssyntax, primFunc_ssyntax,
//?   string pat = toString(car(args));
//?   list
//?   Cell* result = car(cdr(args));
//? )
