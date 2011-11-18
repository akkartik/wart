COMPILE_PRIM_FUNC(string_splice, primFunc_string_splice, "($string $start $end $val)",
  Cell* str = lookup("$string");
  if (!isString(str)) {
    warn << "can't set non-string: " << str << endl;
    return nil;
  }

  size_t start = toNum(lookup("$start"));
  size_t end = toNum(lookup("$end"));
  if (start > ((string*)str->car)->length()) { // append works
    warn << "string too short: " << str << " " << start << endl;
    return nil;
  }

  Cell* val = lookup("$val");
  if (!isString(val))
    warn << "can't set string with non-string: " << val << endl;
  ((string*)str->car)->replace(start, end-start, toString(val));
  return mkref(val);
)

COMPILE_PRIM_FUNC(string_get, primFunc_string_get, "($string $index $end)",
  Cell* str = lookup("$string");
  if (!isString(str)) {
    warn << "not a string: " << str << endl;
    return nil;
  }

  size_t index = toNum(lookup("$index"));
  if (index > ((string*)str->car)->length()-1)
    return nil;

  size_t end = (lookup("$end") != nil) ? toNum(lookup("$end")) : index+1;
  if (end > ((string*)str->car)->length()) {
    warn << "no such end-index in string: " << str << " " << end << endl;
    return nil;
  }

  return mkref(newString(toString(str).substr(index, end-index)));
)

COMPILE_PRIM_FUNC(string_to_sym, primFunc_string_to_sym, "($s)",
  return mkref(newSym(toString(lookup("$s"))));
)
