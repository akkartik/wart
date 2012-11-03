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
//  read the minimum possible from the stream for interactive repls

list<Token> nextExpr(istream& i) {
  list<Token> result;
  if (i.eof()) return result;

  IndentSensitiveStream in(i);
  long openExplicitParens = 0;  // parens in the original
  stack<long> implicitParenStack;   // parens we inserted

  if (!in.atStartOfLine)
    // we're in the middle of a line,
    // because there were multiple exprs on a single line
    return indentInsensitiveExpr(in);

  long thisLineIndent = skipInitialNewlinesToFirstIndent(in);

  list<Token> buffer;   // when you might need to insert an implicit paren
  long numWordsInLine = 0;
  bool parenAtStartOfLine = false;
  while (!in.fd.eof()) {
    Token curr = nextToken(in);
    if (curr.newline) {
      if (interactive && openExplicitParens == 0
          && (implicitParenStack.empty() || in.fd.peek() == '\n'))
        break;
    }
    else if (curr.isQuoteOrUnquote()) {
      if (numWordsInLine < 2)
        buffer.push_back(curr);
      else
        emit(curr, result, openExplicitParens);
    }
    else if (curr.isParen()) {
      if (!parenAtStartOfLine)
        parenAtStartOfLine = (curr == "(" && numWordsInLine == 0);
      if (numWordsInLine < 2 && openExplicitParens == 0 && !parenAtStartOfLine) {
        buffer.push_back(curr);
      }
      else {
        emitAll(buffer, result, openExplicitParens);
        emit(curr, result, openExplicitParens);
        if (openExplicitParens == 0 && implicitParenStack.empty())
          break;
      }
    }
    else if (!curr.isIndent()) { // curr is a 'word' token
      ++numWordsInLine;
      if (numWordsInLine < 2) {
        buffer.push_back(curr);
      }
      else if (numWordsInLine == 2) {
        if (openExplicitParens == 0 && !parenAtStartOfLine) {
          result.push_back(Token("("));
          implicitParenStack.push(thisLineIndent);
        }
        emitAll(buffer, result, openExplicitParens);
        emit(curr, result, openExplicitParens);
      }
      else {  // later words
        emit(curr, result, openExplicitParens);
      }
    }
    else { // curr.isIndent()
      long nextLineIndent = curr.indentLevel;
      emitAll(buffer, result, openExplicitParens);
      while (!implicitParenStack.empty() && nextLineIndent <= implicitParenStack.top()) {
        result.push_back(Token(")"));
        implicitParenStack.pop();
      }

      if (implicitParenStack.empty() && openExplicitParens == 0) {
        restoreIndent(nextLineIndent, in);
        break;
      }

      // reset
      thisLineIndent = nextLineIndent;
      numWordsInLine = 0;
      parenAtStartOfLine = false;
    }
  }

  emitAll(buffer, result, openExplicitParens);
  for (unsigned long i=0; i < implicitParenStack.size(); ++i)
    result.push_back(Token(")"));
  return result;
}



// Internals.

void emit(Token& t, list<Token>& out, long& openExplicitParens) {
  out.push_back(t);
  if (t == "(") ++openExplicitParens;
  if (t == ")") --openExplicitParens;
  if (openExplicitParens < 0) RAISE << "Unbalanced )" << endl;
}

void emitAll(list<Token>& buffer, list<Token>& out, long& openExplicitParens) {
  for (list<Token>::iterator p = buffer.begin(); p != buffer.end(); ++p)
    emit(*p, out, openExplicitParens);
  buffer.clear();
}

void restoreIndent(long indent, IndentSensitiveStream& in) {
  if (in.fd.eof()) return;
  for (int i = 0; i < indent; ++i)
    in.fd.putback(' ');
  in.atStartOfLine = true;
}

list<Token> indentInsensitiveExpr(IndentSensitiveStream& in) {
  list<Token> result;
  long openExplicitParens = 0;
  while (!in.fd.eof()) {
    Token curr = nextToken(in);
    if (curr.newline || curr.isIndent()) {
    }
    else if (curr.isQuoteOrUnquote()) {
      result.push_back(curr);
    }
    else if (curr == "(") {
      result.push_back(curr);
      ++openExplicitParens;
    }
    else if (curr == ")") {
      result.push_back(curr);
      --openExplicitParens;
      if (openExplicitParens == 0) break;
    }
    else { // word
      result.push_back(curr);
      if (openExplicitParens == 0) break;
    }
  }
  return result;
}

#include<assert.h>

long skipInitialNewlinesToFirstIndent(IndentSensitiveStream& in) {
  for (;;) {
    Token token = nextToken(in);
    if (token.isIndent()) return token.indentLevel;
    assert(token.newline);
  }
}
