//// construct parse tree out of cells

cell* next_cell(indent_sensitive_stream& in) {
  return build_cell(transform_infix(next_ast_node(in)));
}

cell* build_cell(ast_node n) {
  new_trace_frame("cell");
  if (n == "") return nil;  // void

  if (is_nil(n)) {
    trace("cell") << "nil";
    return nil;
  }
  if (is_list(n) && n.elems.front() == ")") {
    if (n.elems.size() > 1) RAISE << "Syntax error: ) not at end of expr\n" << die();
    trace("cell") << "nil";
    return nil;
  }

  if (is_atom(n)) {
    errno = 0;
    char* end;
    long v = strtol(n.atom.value.c_str(), &end, 0);
    if (*end == '\0' && errno == 0) {
      trace("cell") << "num: " << v;
      return new_num(v);
    }

    if (errno == ERANGE || errno == EOVERFLOW)
      RAISE << "dropping precision for bignum " << n.atom.value << '\n';

    float f = strtof(n.atom.value.c_str(), &end);
    if (*end == '\0') {
      trace("cell") << "float: " << f;
      if (n.atom.value.substr(0, 2) == "-.")
        RAISE << "assuming '" << n.atom.value << "' is a float; to remove this warning say '-0" << n.atom.value.substr(1) << "'.\n"
            << "If you mean to negate an int, skip the ssyntax: '-" << n.atom.value.substr(2) << "'.\n";
      return new_num(f);
    }

    if (n.atom.value.c_str()[0] == '"') {
      trace("cell") << "string: " << n.atom.value;
      return new_string(n.atom.value.substr(1, n.atom.value.length()-2));
    }

    trace("cell") << "sym: " << n.atom.value;
    return new_sym(n.atom.value);
  }

  list<ast_node>::iterator first = n.elems.begin();
  if (*first == "(") {
    n.elems.pop_front();
    cell* result = build_cell(n);
    trace("cell") << result;
    return result;
  }

  cell* new_form = new_cell();
  set_car(new_form, build_cell(n.elems.front()));

  list<ast_node>::iterator next = first; ++next;
  if (*next == "...") {
    if (next != --n.elems.end())
      set_cdr(new_form, build_cell(*++next));  // dotted pair
    else
      set_cdr(new_form, build_cell(*next));
  }
  else if (is_quote_or_unquote(*first) && n.elems.size() == 2) {
    set_cdr(new_form, build_cell(*next));  // dotted pair
  }
  else {
    n.elems.pop_front();
    if (n.elems.empty())
      RAISE << "Error in parsing " << n << '\n' << die();
    set_cdr(new_form, build_cell(n));
  }

  trace("cell") << new_form;
  return new_form;
}

bool is_nil(const ast_node& n) {
  return n.atom == "nil"
      || (n.elems.size() == 2 && n.elems.front() == "(" && n.elems.back() == ")");
}
