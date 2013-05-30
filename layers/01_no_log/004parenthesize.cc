//// insert explicit parens based on indentation

// Design considered the following:
//  keywords in other languages to look different from functions: def, if, while, etc.
//  fully-parenthesized expressions to not be messed with
//    so ignore indent when lines start with parens
//    so ignore indent inside parens
//    so no way to disable this pass
//  introduce no new operators
//    so wart doesn't use nested lists like scheme's cond
//    so lines with one word are never wrapped in parens, like x or ,f.sym
//  encourage macros to fully parenthesize
//    so ignore indent inside backquote
//  support interactive repls
//    so read the minimum possible from the stream
//    be robust to leading whitespace and empty lines

#include<assert.h>

list<token> next_expr(indent_sensitive_stream& in) {
  list<token> result;   // emit tokens here
  if (in.eof()) return result;

  if (!in.at_start_of_line) {
    // we're in the middle of a line,
    // because there were multiple exprs on a single line
    result = indent_insensitive_expr(in);
    if (!result.empty()) return result;
  }

  if (in.eof()) return result;
  assert(in.at_start_of_line);

  //// rule: insert implicit paren before lines that:
  ////   a) have 2 or more words
  ////   b) aren't already parenthesized at the front
  //// both conditions are oblivious of quotes and unquotes
  list<token> buffer;
  long num_words_in_line = 0;
  bool paren_at_start_of_line = false;
  ////   c) aren't inside open parens
  long explicit_open_parens = 0;  // parens in the original
  stack<long> implicit_open_parens;   // parens we inserted with their indent levels

  long this_line_indent = skip_initial_newslines_to_first_indent(in);
  while (!in.eof()) {
    token curr = next_token(in);

    //// various book-keeping based on curr token
    if (is_indent(curr) || curr.newline)
      num_words_in_line = 0;
    else if (is_word(curr))
      ++num_words_in_line;

    if (is_indent(curr))
      this_line_indent = curr.indent_level;

    if (is_indent(curr) || curr.newline)
      paren_at_start_of_line = false;
    else if (is_open_paren(curr) && num_words_in_line == 0)
      paren_at_start_of_line = true;

    //// decide what to emit, tracking (implicit/explicit) open parens
    if (paren_at_start_of_line || is_indent(curr) || curr.newline)
      emit_all(buffer, curr, result, explicit_open_parens);
    else if (num_words_in_line < 2)
      buffer.push_back(curr);
    else {
      if (num_words_in_line == 2 && is_word(curr) && explicit_open_parens == 0) {
        result.push_back(token("("));
        implicit_open_parens.push(this_line_indent);
      }
      emit_all(buffer, curr, result, explicit_open_parens);
    }

    if (is_indent(curr)) {
      while (!implicit_open_parens.empty() && implicit_open_parens.top() >= this_line_indent) {
        result.push_back(token(")"));
        implicit_open_parens.pop();
      }
    }

    //// we done?
    if (explicit_open_parens == 0 && implicit_open_parens.empty()) {
      if (is_indent(curr)) restore_indent(this_line_indent, in);
      if (is_indent(curr) || curr.newline || curr == ")") break;
    }
    if (Interactive && curr.newline && explicit_open_parens == 0 && in.fd.peek() == '\n')
      break;
  }

  emit_all(buffer, /*dummy*/token::Newline(), result, explicit_open_parens);
  for (unsigned long i=0; i < implicit_open_parens.size(); ++i)
    result.push_back(token(")"));
  return result;
}



//// internals

void emit(const token& t, list<token>& out, long& explicit_open_parens) {
  if (t.newline || is_indent(t)) return;
  out.push_back(t);
  if (is_open_paren(t)) ++explicit_open_parens;
  if (is_close_paren(t)) --explicit_open_parens;
  if (explicit_open_parens < 0) RAISE << "Unbalanced )\n";
}

void emit_all(list<token>& buffer, const token& curr, list<token>& out, long& explicit_open_parens) {
  for (list<token>::const_iterator p = buffer.begin(); p != buffer.end(); ++p)
    emit(*p, out, explicit_open_parens);
  buffer.clear();
  emit(curr, out, explicit_open_parens);
}

void restore_indent(long indent, indent_sensitive_stream& in) {
  if (in.eof()) return;
  for (int i = 0; i < indent; ++i)
    in.fd.putback(' ');
  in.at_start_of_line = true;
}

list<token> indent_insensitive_expr(indent_sensitive_stream& in) {
  list<token> result;
  long explicit_open_parens = 0;
  while (!in.eof()) {
    token curr = next_token(in);
    if (curr.newline) {
      assert(in.at_start_of_line);
      if (explicit_open_parens == 0) break;
    }
    else if (is_indent(curr)) {
    }
    else if (is_quote_or_unquote(curr)) {
      result.push_back(curr);
    }
    else if (is_open_paren(curr)) {
      result.push_back(curr);
      ++explicit_open_parens;
    }
    else if (is_close_paren(curr)) {
      result.push_back(curr);
      --explicit_open_parens;
      if (explicit_open_parens == 0) break;
    }
    else { //// word
      result.push_back(curr);
      if (explicit_open_parens == 0) break;
    }
  }
  return result;
}

long skip_initial_newslines_to_first_indent(indent_sensitive_stream& in) {
  for (;;) {
    token token = next_token(in);
    if (is_indent(token)) return token.indent_level;
    assert(token.newline);
  }
}

bool is_indent(const token& t) {
  return t.value == "" && !t.newline;
}

bool is_paren(const token& t) {
  return is_open_paren(t) || is_close_paren(t);
}

bool is_open_paren(const token& t) {
  return t == "(";
}
bool is_close_paren(const token& t) {
  return t == ")";
}

bool is_quote_or_unquote(const token& t) {
  return t == "'" || t == "`"
      || t == "," || t == ",@" || t == "@";
}

bool is_word(const token& t) {
  return !t.newline && !is_indent(t) && !is_paren(t) && !is_quote_or_unquote(t);
}
