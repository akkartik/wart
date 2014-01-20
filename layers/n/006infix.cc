//// transform infix expressions into prefix

// Design considered the following:
//  intuitive arithmetic: a + b
//  user-defined ops
//  parsing in macros: `(,a + ,b) => `(+ ,a ,b)
//    so no precedence
//  easy combination
//    so permit infix without whitespace: a*b, n+1
//    so no-whitespace takes precedence over whitespace: a*b + c, n * n-1
//    so symbols can't include operator chars
//  easy to explain
//    so ops are always left-associative: a+b+c
//    so prefix ops are left-associative: (-a + b)
//  compounds involving function calls: (f a+b)
//    so infix takes precedence over prefix call
//  convenient range comparison: (0 <= k < n)
//    so comparers should return last arg on success: (1 < 3) => 3
//    so comparers should return any nil passed in (1 < nil) => nil; (nil < 1) => nil
//  interaction with paren insertion
//  support a.-b, l.-1
//    rely on separate def for .- op

const string Extra_sym_chars = "$?!_:";  // besides letters and digits

ast_node transform_infix(ast_node n) {
  new_trace_frame("infix");
  // special-case: ellipses are for dotted lists, not infix
  if (is_atom(n) && n.atom.value == "...") {
    trace("infix") << "ignoring ellipses " << n;
    return n;
  }

  if (is_atom(n) && n.atom.value[0] == '\"') {
    trace("infix") << "string: " << n;
    return n;
  }

  if (is_atom(n) && is_parseable_as_float(n.atom.value)) {
    trace("infix") << "skipping float " << n;
    return n;
  }

  if (is_atom(n) && !contains_infix_char(n.atom.value)) {
    trace("infix") << "skipping " << n;
    return n;
  }

  if (is_atom(n))
    n = tokenize_infix(n);
  if (is_atom(n)) {
    trace("infix") << "didn't tokenize " << n;
    return n;
  }

  if (is_quote_or_unquote(n.elems.front())) {
    list<ast_node>::iterator p = n.elems.begin();
    while (is_quote_or_unquote(*p)) {
      trace("infix") << "skipping past " << *p;
      ++p;
      if (p == n.elems.end()) {
        trace("infix") << "just quotes/unquotes " << n;
        return n;
      }
    }
    ast_node result = (p == --n.elems.end())
        ? transform_infix(*p)
        : transform_infix(ast_node(list<ast_node>(p, n.elems.end())));
    if (is_atom(result)) {
      n.elems.erase(p, n.elems.end());
      n.elems.push_back(result);
      return n;
    }
    else {
      result.elems.insert(result.elems.begin(), n.elems.begin(), p);
      return result;
    }
  }

  if (n.elems.front() != token("(")) {
    trace("infix") << "not a list " << n;
    return n;
  }

  if (n.elems.size() == 2) {
    trace("infix") << "nil: " << n;
    return n;
  }

  if (is_infix_call_without_args(n)) {
    trace("infix") << "extracting solo infix op from " << n;
    return *++n.elems.begin();
  }

  size_t old_size = n.elems.size();

  // now n is guaranteed to have at least 3 ops
  // slide a window of 3, pinching into s-exprs when middle elem is an op
  list<ast_node>::iterator prev = n.elems.begin();
  list<ast_node>::iterator curr=prev;  ++curr;
  list<ast_node>::iterator next=curr;  ++next;
  for (; next != n.elems.end(); ++prev, ++curr, ++next) {
    if (curr->atom.value == "...") continue;

    if (!is_infix_op(*curr)) {
      *curr = transform_infix(*curr);
      continue;
    }
    if (*next == ")") {  //// postfix op
      *curr = transform_infix(*curr);
      continue;
    }

    list<ast_node> tmp;
    tmp.push_back(transform_infix(*curr));
    if (prev == n.elems.begin()) {
      //// prefix op; grab as many non-ops as you can
      list<ast_node>::iterator oldnext = next;
      while (!is_infix_op(*next)) {
        tmp.push_back(transform_infix(*next));
        ++next;
        if (next == --n.elems.end()) break;
      }

      // update next
      n.elems.erase(oldnext, next);
      next=curr;  ++next;
    }
    else {
      //// infix op; switch to prefix
      tmp.push_back(*prev);
      tmp.push_back(transform_infix(*next));

      // update both prev and next
      n.elems.erase(prev);
      prev=curr;  --prev;
      n.elems.erase(next);
      next=curr;  ++next;
    }
    // wrap in parens
    tmp.push_front(ast_node(token("(")));
    tmp.push_back(ast_node(token(")")));
    // insert the new s-expr
    *curr = ast_node(tmp);
  }

  // (a + b) will have become ((+ a b)); strip out one pair of parens
  if (n.elems.size() == 3 && old_size > 3) {
    trace("infix") << "munging solo pinched result " << n;
    trace("infix") << "=> " << *++n.elems.begin();
    return *++n.elems.begin();
  }

  trace("infix") << "=> " << n;
  return n;
}

ast_node tokenize_infix(ast_node n) {
  const char* var = n.atom.value.c_str();

  string out;
  for (size_t x=0; var[x] != '\0'; ++x) {
    if (isdigit(var[x]) && (x == 0 || is_infix_char(var[x-1]))) {
      const char* next = skip_float(&var[x]);
      if (next != &var[x]) {
        out += " ";
        while (var[x] != '\0' && &var[x] != next) {
          out += var[x];
          ++x;
        }
        --x;
        continue;
      }
    }

    if ((x > 0)
          && ((is_infix_char(var[x]) && is_regular_char(var[x-1]) && var[x-1] != '$')
              || (is_regular_char(var[x]) && is_infix_char(var[x-1])))) {
      out += " ";
    }
    out += var[x];
  }
  stringstream ss(out);
  indent_sensitive_stream tmp(ss);
  return next_ast_node(tmp);
}



bool is_infix_op(ast_node n) {
  if (is_list(n)) return false;
  if (n == "...") return false;
  string s = n.atom.value;
  string::iterator p = s.begin();
  if (*p != '$' && !is_infix_char(*p))
    return false;
  for (++p; p != s.end(); ++p)
    if (!is_infix_char(*p))
      return false;
  return true;
}

bool contains_infix_char(string name) {
  for (string::iterator p = name.begin(); p != name.end(); ++p)
    if (is_infix_char(*p))
      return true;
  return false;
}

bool is_infix_char(char c) {
  return !find(Punctuation_chars, c)
      && !find(Quote_and_unquote_chars, c)
      && !isalnum(c) && !find(Extra_sym_chars, c);
}

bool is_regular_char(char c) {
  return isalnum(c) || find(Extra_sym_chars, c);
}

bool is_infix_call_without_args(ast_node n) {
  if (!is_list(n) || n.elems.size() != 3) return false;
  list<ast_node>::iterator p = n.elems.begin();
  if (*p != token("(")) return false;
  ++p;
  return is_infix_op(*p);
}

bool is_parseable_as_float(string s) {
  errno = 0;
  const char* end = skip_float(s.c_str());
  return *end == '\0' && errno == 0;
}

const char* skip_float(const char* s) {
  char* end = NULL;
  unused float dummy = strtof(s, &end);
  return end;
}
