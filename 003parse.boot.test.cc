void test_parse_handles_empty_input() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L""))));
  check(ast.empty());
}

void test_parse_handles_trailing_comments() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L"; ab"))));
  check(ast.empty());
}

void test_parse_handles_atom() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L"34"))));
  list<AstNode>::iterator p = ast.begin();
  check_eq(*p, Token::of(L"34")); ++p;
  check(p == ast.end());
}

void test_parse_handles_atoms() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L"34\n\"a b c\""))));
  list<AstNode>::iterator p = ast.begin();
  check_eq(*p, Token::of(L"34")); ++p;
  check_eq(*p, Token::of(L"\"a b c\"")); ++p;
  check(p == ast.end());
}

void test_parse_handles_forms() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L"34 \"a b c\""))));
  check_eq(ast.size(), 1);
  check(ast.front().isList());
  list<AstNode>::iterator p = ast.front().elems.begin();
  check_eq(*p, Token::of(L"(")); ++p;
  check_eq(*p, Token::of(L"34")); ++p;
  check_eq(*p, Token::of(L"\"a b c\"")); ++p;
  check_eq(*p, Token::of(L")")); ++p;
  check(p == ast.front().elems.end());
}

void test_parse_handles_nested_forms() {
  list<AstNode> ast = parse(parenthesize(tokenize(teststream(L"34 (2 3) \"a b c\""))));
  check_eq(ast.size(), 1);
  check(ast.front().isList());
  list<AstNode>::iterator p = ast.front().elems.begin();
  check_eq(*p, Token::of(L"(")); ++p;
  check_eq(*p, Token::of(L"34")); ++p;
  check(p->isList());
    list<AstNode> ast2 = p->elems; ++p;
    list<AstNode>::iterator q = ast2.begin();
    check_eq(*q, Token::of(L"(")); ++q;
    check_eq(*q, Token::of(L"2")); ++q;
    check_eq(*q, Token::of(L"3")); ++q;
    check_eq(*q, Token::of(L")")); ++q;
    check(q == ast2.end());

  check_eq(*p, Token::of(L"\"a b c\"")); ++p;
  check_eq(*p, Token::of(L")")); ++p;
  check(p == ast.front().elems.end());
}
