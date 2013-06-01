void test_infix_passes_atoms() {
  read_all("abc");
  CHECK_TRACE_CONTENTS("infix", "skipping abc");
}

void test_infix_passes_ellipses() {
  read_all("...");
  CHECK_TRACE_CONTENTS("infix", "ignoring ellipses ...");
}

void test_infix_passes_atoms2() {
  read_all("-3.2");
  CHECK_TRACE_CONTENTS("infix", "skipping float -3.2");
}

void test_infix_passes_strings() {
  read_all("\"a b+c\"");
  CHECK_TRACE_CONTENTS("infix", "string: \"a b+c\"");
}

void test_infix_passes_floats() {
  read_all("2e-2");
  CHECK_TRACE_CONTENTS("infix", "skipping float 2e-2");
}

void test_infix_passes_dollar_vars() {
  read_all("$a");
  CHECK_TRACE_CONTENTS("infix", "skipping $a");
}

void test_infix_handles_dollar_op() {
  read_all("$+");
  CHECK_TRACE_CONTENTS("infix", "didn't tokenize $+");
}

void test_infix_handles_op_without_args() {
  TEMP(x, read("(+)"));
  CHECK_TRACE_CONTENTS("infix", "extracting solo infix op from (+)");
  CHECK_EQ(x, new_sym("+"));
}

void test_infix_handles_op_without_args2() {
  read_all("(= (+) 3)");
  CHECK_TRACE_CONTENTS("infix", "=> (= + 3)");
}

void test_infix_handles_dollar_op_without_args() {
  indent_sensitive_stream in("($+)");
  ast_node n = transform_infix(next_ast_node(in));
  CHECK(is_atom(n));
  CHECK_EQ(n.atom, "$+");
  CHECK_TRACE_CONTENTS("infix", "extracting solo infix op from ($+)");
}

void test_infix_handles_quoting() {
  read_all("',a");
  CHECK_TRACE_CONTENTS("infix", "skipping past 'skipping past ,");
}

void test_infix_handles_quoting2() {
  read_all("',a+b");
  CHECK_TRACE_CONTENTS("infix", "skipping past 'skipping past ,=> (+ a b)");
}

void test_infix_handles_quoting3() {
  read_all("',(a + b)");
  CHECK_TRACE_CONTENTS("infix", "skipping past 'skipping past ,=> (+ a b)");
}

void test_infix_handles_quoting4() {
  read_all("',(+)");
  CHECK_TRACE_CONTENTS("infix", "skipping past 'skipping past ,extracting solo infix op from (+)");
}

void test_infix_handles_simple_lists() {
  read_all("(a + b)");
  CHECK_TRACE_TOP("infix", "=> (+ a b)");
}

void test_infix_passes_wrapped_op() {
  read_all("(a (+) b)");
  CHECK_TRACE_TOP("infix", "=> (a + b)");
}

void test_infix_handles_infix_ops_in_prefix() {
  read_all("(+ a b)");
  CHECK_TRACE_TOP("infix", "=> (+ a b)");
}

void test_infix_passes_ellipses_in_infix() {
  read_all("(a ... b)");
  CHECK_TRACE_TOP("infix", "=> (a ... b)");
}

void test_infix_passes_op_with_ellipses() {
  read_all("(+ ... b)");
  CHECK_TRACE_TOP("infix", "=> (+ ... b)");
}

void test_infix_handles_infix_ops_in_unary_prefix() {
  read_all("(+ a)");
  CHECK_TRACE_TOP("infix", "=> (+ a)");
}

void test_infix_handles_infix_ops_in_nested_lists() {
  read_all("((a + b))");
  CHECK_TRACE_TOP("infix", "=> ((+ a b))");
}

void test_infix_handles_infix_ops_in_nested_lists2() {
  read_all("(do (a + b))");
  CHECK_TRACE_TOP("infix", "=> (do (+ a b))");
}

void test_infix_handles_infix_ops_in_nested_lists3() {
  read_all("(a = (a + 1))");
  CHECK_TRACE_TOP("infix", "=> (= a (+ a 1))");
}

void test_infix_left_associates() {
  read_all("(a + b + c)");
  CHECK_TRACE_TOP("infix", "=> (+ (+ a b) c)");
}

void test_infix_always_has_higher_precedence_than_call() {
  read_all("(f a + b)");
  CHECK_TRACE_TOP("infix", "=> (f (+ a b))");
}

void test_infix_always_has_lower_precedence_than_prefix() {
  read_all("-a+b");
  CHECK_TRACE_TOP("infix", "=> (+ (- a) b)");
}

void test_infix_handles_multiple_infix_ops() {
  read_all("(f a + b c + d)");
  CHECK_TRACE_TOP("infix", "=> (f (+ a b) (+ c d))");
}

void test_infix_handles_op_without_spaces() {
  read_all("a+b");
  CHECK_TRACE_TOP("infix", "=> (+ a b)");
}

void test_infix_handles_op_without_spaces2() {
  read_all("$a+b");
  CHECK_TRACE_TOP("infix", "=> (+ $a b)");
}

void test_infix_handles_op_with_float() {
  read_all("a+1.0");
  CHECK_TRACE_TOP("infix", "=> (+ a 1.0)");
}

void test_infix_handles_op_with_float2() {
  read_all("3.0+1.0");
  CHECK_TRACE_TOP("infix", "=> (+ 3.0 1.0)");
}

void test_infix_handles_op_with_float3() {
  read_all("a3.b");
  CHECK_TRACE_TOP("infix", "=> (. a3 b)");
}

void test_infix_gives_ops_without_spaces_precedence() {
  read_all("(n * n-1)");
  CHECK_TRACE_TOP("infix", "=> (* n (- n 1))");
}

void test_infix_handles_complement_as_usual() {
  read_all("~a.b");
  CHECK_TRACE_TOP("infix", "=> (. (~ a) b)");
}

void test_infix_handles_backquote() {
  read_all("`(a + b)");
  CHECK_TRACE_CONTENTS("infix", "=> (+ a b)");  // not in top frame
}

void test_infix_handles_unquote_splice() {
  read_all(",@a+b");
  CHECK_TRACE_CONTENTS("infix", "=> (+ a b)");  // not in top frame
}

void test_infix_handles_unquote_splice2() {
  read_all(",@(a + b)");
  CHECK_TRACE_CONTENTS("infix", "=> (+ a b)");  // not in top frame
}
