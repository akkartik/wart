//// compiled primitives for strings

COMPILE_FN(string_range, compiledfn_string_get, "($string $index $end)",
  cell* str = lookup("$string");
  if (!is_string(str)) {
    RAISE << "not a string: " << str << '\n';
    return nil;
  }

  size_t index = to_int(lookup("$index"));
  if (index > ((string*)str->car)->length()-1)
    return nil;

  size_t end = to_int(lookup("$end"));
  if (end > ((string*)str->car)->length()) {
    RAISE << "no such end-index in string: " << str << " " << end << '\n';
    return nil;
  }

  return mkref(new_string(to_string(str).substr(index, end-index)));
)

COMPILE_FN(string_splice, compiledfn_string_splice, "($string $start $end $val)",
  cell* str = lookup("$string");
  if (!is_string(str)) {
    RAISE << "can't set non-string: " << str << '\n';
    return nil;
  }

  size_t start = to_int(lookup("$start"));
  size_t end = to_int(lookup("$end"));
  if (start > ((string*)str->car)->length()) {  // append works
    RAISE << "string too short: " << str << " " << start << '\n';
    return nil;
  }

  cell* val = lookup("$val");
  if (!is_string(val))
    RAISE << "can't set string with non-string: " << val << '\n';
  ((string*)str->car)->replace(start, end-start, to_string(val));
  return mkref(val);
)

COMPILE_FN(string_to_list, compiledfn_string_to_list, "($string)",
  string str = to_string(lookup("$string"));
  cell* p_result = new_cell();
  char x[2];
  x[1] = 0;
  cell* curr = p_result;
  for (size_t i = 0; i < str.size(); ++i) {
    x[0] = str[i];
    add_cons(curr, new_string(x));
    curr=cdr(curr);
  }
  return drop_ptr(p_result);
)

COMPILE_FN(string_to_sym, compiledfn_string_to_sym, "($s)",
  string s = to_string(lookup("$s"));
  if (s == "") return nil;
  return mkref(new_sym(s));
)

COMPILE_FN(string_lesser, compiledfn_string_lesser, "($x $y)",
  cell* x = lookup("$x");
  cell* y = lookup("$y");
  if (x == nil || y == nil) return nil;
  return to_string(x) < to_string(y) ? mkref(y) : nil;
)

COMPILE_FN(split, compiledfn_split, "($s $delim)",
  cell* d = lookup("$delim");
  char delim = (d != nil) ? to_string(d)[0] : ' ';
  stringstream ss(to_string(lookup("$s")));
  cell* p_result = new_cell();
  for (cell* curr = p_result; !ss.eof(); curr=cdr(curr)) {
    string word;
    getline(ss, word, delim);
    add_cons(curr, new_string(word));
  }
  return drop_ptr(p_result);
)

COMPILE_FN(upcase, compiledfn_upcase, "($s)",
  string s = to_string(lookup("$s"));
  std::transform(s.begin(), s.end(), s.begin(), toupper);
  return mkref(new_string(s));
)

COMPILE_FN(downcase, compiledfn_downcase, "($s)",
  string s = to_string(lookup("$s"));
  std::transform(s.begin(), s.end(), s.begin(), tolower);
  return mkref(new_string(s));
)
