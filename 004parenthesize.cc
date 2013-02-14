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

list<Token> nextExpr(IndentSensitiveStream& in) {
  list<Token> result;   // emit tokens here
  if (in.eof()) return result;

  if (!in.atStartOfLine) {
    // we're in the middle of a line,
    // because there were multiple exprs on a single line
    result = indentInsensitiveExpr(in);
    if (!result.empty()) return result;
  }

  if (in.eof()) return result;
  assert(in.atStartOfLine);

  //// rule: insert implicit paren before lines that:
  ////   a) have 2 or more words
  ////   b) aren't already parenthesized at the front
  //// both conditions are oblivious of quotes and unquotes
  list<Token> buffer;
  long numWordsInLine = 0;
  bool parenAtStartOfLine = false;
  ////   c) aren't inside open parens
  long explicitOpenParens = 0;  // parens in the original
  stack<long> implicitOpenParens;   // parens we inserted with their indent levels

  long thisLineIndent = skipInitialNewlinesToFirstIndent(in);
  while (!in.eof()) {
    Token curr = nextToken(in);

    //// various book-keeping based on curr token
    if (isIndent(curr))
      numWordsInLine = 0;
    else if (isWord(curr))
      ++numWordsInLine;

    if (isIndent(curr))
      thisLineIndent = curr.indentLevel;

    if (isIndent(curr))
      parenAtStartOfLine = false;
    else if (curr == "(" && numWordsInLine == 0)
      parenAtStartOfLine = true;

    //// decide what to emit, tracking (implicit/explicit) open parens
    if (parenAtStartOfLine) {
      emitAll(buffer, result, explicitOpenParens);  // any quotes
      emit(curr, result, explicitOpenParens);   // numWordsInLine is irrelevant
    }
    else if (numWordsInLine < 2) {
      buffer.push_back(curr);
    }
    else {
      if (numWordsInLine == 2 && isWord(curr) && explicitOpenParens == 0 && !parenAtStartOfLine) {
        result.push_back(Token("("));
        implicitOpenParens.push(thisLineIndent);
      }
      emitAll(buffer, result, explicitOpenParens);
      emit(curr, result, explicitOpenParens);
    }

    if (isIndent(curr)) {
      emitAll(buffer, result, explicitOpenParens);
      while (!implicitOpenParens.empty() && thisLineIndent <= implicitOpenParens.top()) {
        result.push_back(Token(")"));
        implicitOpenParens.pop();
      }
    }

    //// we done?
    if (explicitOpenParens == 0 && implicitOpenParens.empty()) {
      if (isIndent(curr)) restoreIndent(thisLineIndent, in);
      if (isIndent(curr) || curr.newline || curr == ")") break;
    }
    if (interactive && curr.newline && explicitOpenParens == 0 && in.fd.peek() == '\n')
      break;
  }

  emitAll(buffer, result, explicitOpenParens);
  for (unsigned long i=0; i < implicitOpenParens.size(); ++i)
    result.push_back(Token(")"));
  return result;
}



//// internals

void emit(Token& t, list<Token>& out, long& explicitOpenParens) {
  if (t.newline || isIndent(t)) return;
  out.push_back(t);
  if (t == "(") ++explicitOpenParens;
  if (t == ")") --explicitOpenParens;
  if (explicitOpenParens < 0) RAISE << "Unbalanced )" << endl;
}

void emitAll(list<Token>& buffer, list<Token>& out, long& explicitOpenParens) {
  for (list<Token>::iterator p = buffer.begin(); p != buffer.end(); ++p)
    emit(*p, out, explicitOpenParens);
  buffer.clear();
}

void restoreIndent(long indent, IndentSensitiveStream& in) {
  if (in.eof()) return;
  for (int i = 0; i < indent; ++i)
    in.fd.putback(' ');
  in.atStartOfLine = true;
}

list<Token> indentInsensitiveExpr(IndentSensitiveStream& in) {
  list<Token> result;
  long explicitOpenParens = 0;
  while (!in.eof()) {
    Token curr = nextToken(in);
    if (curr.newline) {
      assert(in.atStartOfLine);
      if (explicitOpenParens == 0) break;
    }
    else if (isIndent(curr)) {
    }
    else if (isQuoteOrUnquote(curr)) {
      result.push_back(curr);
    }
    else if (curr == "(") {
      result.push_back(curr);
      ++explicitOpenParens;
    }
    else if (curr == ")") {
      result.push_back(curr);
      --explicitOpenParens;
      if (explicitOpenParens == 0) break;
    }
    else { //// word
      result.push_back(curr);
      if (explicitOpenParens == 0) break;
    }
  }
  return result;
}

long skipInitialNewlinesToFirstIndent(IndentSensitiveStream& in) {
  for (;;) {
    Token token = nextToken(in);
    if (isIndent(token)) return token.indentLevel;
    assert(token.newline);
  }
}

bool isIndent(const Token& t) {
  return t.token == "" && !t.newline;
}

bool isParen(const Token& t) {
  return t == "(" || t == ")";
}

bool isQuoteOrUnquote(const Token& t) {
  return t == "'" || t == "`"
      || t == "," || t == ",@" || t == "@";
}

bool isWord(const Token& t) {
  return !t.newline && !isIndent(t) && !isParen(t) && !isQuoteOrUnquote(t);
}
