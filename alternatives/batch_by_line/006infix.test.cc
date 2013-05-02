void test_infix_passes_atoms() {
  stringstream in("abc");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_ellipses() {
  stringstream in("...");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_atoms2() {
  stringstream in("-3.2");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_strings() {
  stringstream in("\"a b+c\"");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_floats() {
  stringstream in("2e-2");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_dollar_vars() {
  stringstream in("$a");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_handles_dollar_op() {
  stringstream in("$+");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  checkEq(transformInfix(nextAstNode(tokens)), "$+");
}

void test_infix_handles_op_without_args() {
  stringstream in("(+)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isAtom(n));
  checkEq(n.atom, "+");
}

void test_infix_handles_op_without_args2() {
  stringstream in("(= (+) 3)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "="); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "3"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_dollar_op_without_args() {
  stringstream in("($+)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isAtom(n));
  checkEq(n.atom, "$+");
}

void test_infix_handles_quoting() {
  stringstream in("',a");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "'"); ++p;
  checkEq(*p, ","); ++p;
  checkEq(*p, "a"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_simple_lists() {
  stringstream in("(a + b)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_wrapped_op() {
  stringstream in("(a (+) b)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_wrapped_op2() {
  stringstream in("(a (:) b)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, ":"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_prefix() {
  stringstream in("(+ a b)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_ellipses_in_infix() {
  stringstream in("(a ... b)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "..."); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_op_with_ellipses() {
  stringstream in("(+ ... b)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "..."); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_unary_prefix() {
  stringstream in("(+ a)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_nested_lists() {
  stringstream in("((a + b))");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  checkEq(n.elems.size(), 3);
  AstNode n2 = *++n.elems.begin();
  check(isList(n2));
  list<AstNode>::iterator p = n2.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n2.elems.end());
}

void test_infix_handles_infix_ops_in_nested_lists2() {
  stringstream in("(do (a + b))");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "do"); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, "("); ++p2;
    checkEq(*p2, "+"); ++p2;
    checkEq(*p2, "a"); ++p2;
    checkEq(*p2, "b"); ++p2;
    checkEq(*p2, ")"); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_infix_ops_in_nested_lists3() {
  stringstream in("(a = (a + 1))");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "="); ++p;
  checkEq(*p, "a"); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, "("); ++p2;
    checkEq(*p2, "+"); ++p2;
    checkEq(*p2, "a"); ++p2;
    checkEq(*p2, "1"); ++p2;
    checkEq(*p2, ")"); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_left_associates() {
  stringstream in("(a + b + c)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, "("); ++p2;
    checkEq(*p2, "+"); ++p2;
    checkEq(*p2, "a"); ++p2;
    checkEq(*p2, "b"); ++p2;
    checkEq(*p2, ")"); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, "c"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op() {
  stringstream in("(a + b)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_always_has_higher_precedence_than_call() {
  stringstream in("(f a + b)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "f"); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, "("); ++p2;
    checkEq(*p2, "+"); ++p2;
    checkEq(*p2, "a"); ++p2;
    checkEq(*p2, "b"); ++p2;
    checkEq(*p2, ")"); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_always_has_lower_precedence_than_prefix() {
  stringstream in("-a+b");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, "("); ++p2;
    checkEq(*p2, "-"); ++p2;
    checkEq(*p2, "a"); ++p2;
    checkEq(*p2, ")"); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_multiple_infix_ops() {
  stringstream in("(f a + b c + d)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "f"); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, "("); ++p2;
    checkEq(*p2, "+"); ++p2;
    checkEq(*p2, "a"); ++p2;
    checkEq(*p2, "b"); ++p2;
    checkEq(*p2, ")"); ++p2;
    check(p2 == n2.elems.end());
  n2 = *p; ++p;
    check(isList(n2));
    p2 = n2.elems.begin();
    checkEq(*p2, "("); ++p2;
    checkEq(*p2, "+"); ++p2;
    checkEq(*p2, "c"); ++p2;
    checkEq(*p2, "d"); ++p2;
    checkEq(*p2, ")"); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_without_spaces() {
  stringstream in("a+b");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_without_spaces2() {
  stringstream in("$a+b");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "$a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_with_float() {
  stringstream in("a+1.0");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "1.0"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_with_float2() {
  stringstream in("3.0+1.0");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "3.0"); ++p;
  checkEq(*p, "1.0"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_op_with_float3() {
  stringstream in("a3.b");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "."); ++p;
  checkEq(*p, "a3"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_gives_ops_without_spaces_precedence() {
  stringstream in("(n * n-1)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "*"); ++p;
  checkEq(*p, "n"); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, "("); ++p2;
    checkEq(*p2, "-"); ++p2;
    checkEq(*p2, "n"); ++p2;
    checkEq(*p2, "1"); ++p2;
    checkEq(*p2, ")"); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_passes_keyword_syms() {
  stringstream in(":a");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isAtom(n));
  checkEq(transformInfix(n), n.atom);
}

void test_infix_passes_keyword_syms2() {
  stringstream in("(f :a x)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "f"); ++p;
  checkEq(*p, ":a"); ++p;
  checkEq(*p, "x"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_complement_as_usual() {
  stringstream in("~a.b");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "("); ++p;
  checkEq(*p, "."); ++p;
  AstNode n2 = *p; ++p;
    check(isList(n2));
    list<AstNode>::iterator p2 = n2.elems.begin();
    checkEq(*p2, "("); ++p2;
    checkEq(*p2, "~"); ++p2;
    checkEq(*p2, "a"); ++p2;
    checkEq(*p2, ")"); ++p2;
    check(p2 == n2.elems.end());
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_backquote() {
  stringstream in("`(a + b)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "`"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_splice() {
  stringstream in("@a+b");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, "@"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}

void test_infix_handles_unquote_splice() {
  stringstream in(",@a+b");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, ",@"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}
void test_infix_handles_unquote_splice2() {
  stringstream in(",@(a + b)");
  list<Token> tokens = insertImplicitParens(tokenize(in));
  AstNode n = transformInfix(nextAstNode(tokens));
  check(isList(n));
  list<AstNode>::iterator p = n.elems.begin();
  checkEq(*p, ",@"); ++p;
  checkEq(*p, "("); ++p;
  checkEq(*p, "+"); ++p;
  checkEq(*p, "a"); ++p;
  checkEq(*p, "b"); ++p;
  checkEq(*p, ")"); ++p;
  check(p == n.elems.end());
}
