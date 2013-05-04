//// compiled primitives for strings

COMPILE_FN(string_range, compiledFn_string_get, "($string $index $end)",
  Cell* str = lookup("$string");
  if (!isString(str)) {
    RAISE << "not a string: " << str << endl;
    return nil;
  }

  size_t index = toInt(lookup("$index"));
  if (index > ((string*)str->car)->length()-1)
    return nil;

  size_t end = toInt(lookup("$end"));
  if (end > ((string*)str->car)->length()) {
    RAISE << "no such end-index in string: " << str << " " << end << endl;
    return nil;
  }

  return mkref(newString(toString(str).substr(index, end-index)));
)

COMPILE_FN(string_splice, compiledFn_string_splice, "($string $start $end $val)",
  Cell* str = lookup("$string");
  if (!isString(str)) {
    RAISE << "can't set non-string: " << str << endl;
    return nil;
  }

  size_t start = toInt(lookup("$start"));
  size_t end = toInt(lookup("$end"));
  if (start > ((string*)str->car)->length()) {  // append works
    RAISE << "string too short: " << str << " " << start << endl;
    return nil;
  }

  Cell* val = lookup("$val");
  if (!isString(val))
    RAISE << "can't set string with non-string: " << val << endl;
  ((string*)str->car)->replace(start, end-start, toString(val));
  return mkref(val);
)

COMPILE_FN(string_to_list, compiledFn_string_to_list, "($string)",
  string str = toString(lookup("$string"));
  Cell* pResult = newCell();
  char x[2];
  x[1] = 0;
  Cell* curr = pResult;
  for (size_t i = 0; i < str.size(); ++i) {
    x[0] = str[i];
    addCons(curr, newString(x));
    curr=cdr(curr);
  }
  return dropPtr(pResult);
)

COMPILE_FN(string_to_sym, compiledFn_string_to_sym, "($s)",
  string s = toString(lookup("$s"));
  if (s == "") return nil;
  return mkref(newSym(s));
)

COMPILE_FN(string_lesser, compiledFn_string_lesser, "($x $y)",
  Cell* x = lookup("$x");
  Cell* y = lookup("$y");
  return toString(x) < toString(y) ? mkref(newNum(1)) : nil;
)

COMPILE_FN(split, compiledFn_split, "($s $delim)",
  Cell* d = lookup("$delim");
  char delim = (d != nil) ? toString(d)[0] : ' ';
  stringstream ss(toString(lookup("$s")));
  Cell* pResult = newCell();
  for (Cell* curr = pResult; !ss.eof(); curr=cdr(curr)) {
    string word;
    getline(ss, word, delim);
    addCons(curr, newString(word));
  }
  return dropPtr(pResult);
)
