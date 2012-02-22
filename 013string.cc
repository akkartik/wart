COMPILE_FN(string_splice, compiledFn_string_splice, "($string $start $end $val)",
  Cell* str = lookup("$string");
  if (!isString(str)) {
    RAISE << "can't set non-string: " << str << endl;
    return nil;
  }

  size_t start = toNum(lookup("$start"));
  size_t end = toNum(lookup("$end"));
  if (start > ((string*)str->car)->length()) { // append works
    RAISE << "string too short: " << str << " " << start << endl;
    return nil;
  }

  Cell* val = lookup("$val");
  if (!isString(val))
    RAISE << "can't set string with non-string: " << val << endl;
  ((string*)str->car)->replace(start, end-start, toString(val));
  return mkref(val);
)

COMPILE_FN(string_range, compiledFn_string_get, "($string $index $end)",
  Cell* str = lookup("$string");
  if (!isString(str)) {
    RAISE << "not a string: " << str << endl;
    return nil;
  }

  size_t index = toNum(lookup("$index"));
  if (index > ((string*)str->car)->length()-1)
    return nil;

  size_t end = toNum(lookup("$end"));
  if (end > ((string*)str->car)->length()) {
    RAISE << "no such end-index in string: " << str << " " << end << endl;
    return nil;
  }

  return mkref(newString(toString(str).substr(index, end-index)));
)

COMPILE_FN(string_to_sym, compiledFn_string_to_sym, "($s)",
  return mkref(newSym(toString(lookup("$s"))));
)

COMPILE_FN(string<, compiledFn_string_lesser, "($x $y)",
  Cell* x = lookup("$x");
  Cell* y = lookup("$y");
  return toString(x) < toString(y) ? mkref(newNum(1)) : nil;
)
