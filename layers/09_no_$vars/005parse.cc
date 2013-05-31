//// construct parse tree out of a list of tokens

struct ast_node {
  token atom;
  list<ast_node> elems;

  explicit ast_node(token t) :atom(t) {}
  explicit ast_node(list<ast_node> l) :elems(l) {}

  bool operator==(const string& x) const {
    return elems.empty() && atom == x;
  }
  bool operator!=(const string& x) const {
    return !(*this == x);
  }
};

ast_node next_ast_node(istream& in) {
  list<token> buffered_tokens = next_expr(in);
  return next_ast_node(buffered_tokens);
}

ast_node next_ast_node(list<token>& in) {
  list<ast_node> subform;
  new_trace_frame("parse");
  if (in.empty()) {
    trace("parse") << ast_node(subform);
    return ast_node(subform);
  }

  subform.push_back(ast_node(next_token(in)));
  while (!in.empty() && is_quote_or_unquote(subform.back().atom))
    subform.push_back(ast_node(next_token(in)));

  if (is_open_paren(subform.back())) {
    while (!in.empty() && subform.back() != ")")
      subform.push_back(next_ast_node(in));
    if (!is_close_paren(subform.back())) RAISE << "Unbalanced (\n"; // << die();
  }

  if (subform.size() == 1) {
    trace("parse") << ast_node(subform.back());
    return ast_node(subform.back());
  }

  trace("parse") << ast_node(subform);
  return ast_node(subform);
}



//// internals

token next_token(list<token>& in) {
  token result = in.front(); in.pop_front();
  return result;
}

bool is_list(const ast_node& n) {
  return !n.elems.empty();
}

bool is_atom(const ast_node& n) {
  return n.elems.empty();
}

bool is_quote_or_unquote(const ast_node& n) {
  return is_atom(n) && is_quote_or_unquote(n.atom);
}

bool is_open_paren(const ast_node& n) {
  return n == "(";
}
bool is_close_paren(const ast_node& n) {
  return n == ")";
}

ostream& operator<<(ostream& os, ast_node x) {
  if (x.elems.empty()) return os << x.atom;
  bool skip_next_space = true;
  for (list<ast_node>::iterator p = x.elems.begin(); p != x.elems.end(); ++p) {
    if (!is_close_paren(*p) && !skip_next_space)
      os << " ";
    os << *p;
    skip_next_space = (is_open_paren(*p) || is_quote_or_unquote(*p));
  }
  return os;
}
