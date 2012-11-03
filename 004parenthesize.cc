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
//  performs flow control at the repl, decides when to show the prompt again

list<Token> nextExpr(istream& in) {
  CodeStream cs(in);
  list<Token> result;
  long openExplicitParens = 0;  // parens in the original
  stack<long> implicitParenStack;   // parens we inserted

  if (cs.fd.eof()) return result;

  if (!cs.atStartOfLine) {
    while (!cs.fd.eof()) {
      Token curr = nextToken(cs);
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
        if (openExplicitParens == 0) return result;
      }
      else { // word
        result.push_back(curr);
        if (openExplicitParens == 0) return result;
      }
    }
    return result;
  }

  long thisLineIndent = skipInitialNewlinesToFirstIndent(cs);

  list<Token> line;
  long numWordsInLine = 0;
  bool parenAtStartOfLine = false;
  while (!cs.fd.eof()) {
    Token curr = nextToken(cs);
    if (curr.newline) {
    }
    else if (curr.isQuoteOrUnquote()) {
      if (numWordsInLine < 2)
        line.push_back(curr);
      else
        result.push_back(curr);
    }
    else if (curr.isParen()) {
      if (!parenAtStartOfLine)
        parenAtStartOfLine = (curr == "(" && numWordsInLine == 0);
      if (numWordsInLine < 2 && openExplicitParens == 0 && !parenAtStartOfLine) {
        line.push_back(curr);
      }
      else {
        for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
          result.push_back(*p);
          if (*p == "(") ++openExplicitParens;
          if (*p == ")") --openExplicitParens;
          if (openExplicitParens < 0) RAISE << "Unbalanced )" << endl;
        }
        line.clear();

        result.push_back(curr);
        if (curr == "(") ++openExplicitParens;
        if (curr == ")") --openExplicitParens;
        if (openExplicitParens < 0) RAISE << "Unbalanced )" << endl;

        if (openExplicitParens == 0 && implicitParenStack.empty())
          break;
      }
    }
    else if (!curr.isIndent()) { // curr is a 'word' token
      ++numWordsInLine;
      if (numWordsInLine < 2) {
        line.push_back(curr);
      }
      else if (numWordsInLine == 2) {
        if (openExplicitParens == 0 && !parenAtStartOfLine) {
          result.push_back(Token("("));
          implicitParenStack.push(thisLineIndent);
        }

        for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
          result.push_back(*p);
          if (*p == "(") ++openExplicitParens;
          if (*p == ")") --openExplicitParens;
          if (openExplicitParens < 0) RAISE << "Unbalanced )" << endl;
        }
        line.clear();

        result.push_back(curr);
      }
      else {
        result.push_back(curr);
      }
    }
    else { // curr.isIndent()
      long nextLineIndent = curr.indentLevel;
      if (!line.empty()) {
        for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
          result.push_back(*p);
          if (*p == "(") ++openExplicitParens;
          if (*p == ")") --openExplicitParens;
          if (openExplicitParens < 0) RAISE << "Unbalanced )" << endl;
        }
        line.clear();
      }

      while (!implicitParenStack.empty() && nextLineIndent <= implicitParenStack.top()) {
        result.push_back(Token(")"));
        implicitParenStack.pop();
      }

      if (implicitParenStack.empty() && openExplicitParens == 0) {
        if (!cs.fd.eof())
          for (int i = 0; i < nextLineIndent; ++i)
            cs.fd.putback(' ');
        cs.atStartOfLine = true;
        break;
      }

      // reset
      thisLineIndent = nextLineIndent;
      numWordsInLine = 0;
      parenAtStartOfLine = false;
    }
  }

  for (list<Token>::iterator p = line.begin(); p != line.end(); ++p)
    result.push_back(*p);
  line.clear();

  for (unsigned long i=0; i < implicitParenStack.size(); ++i)
    result.push_back(Token(")"));
  return result;
}



// Internals.

#include<assert.h>

long skipInitialNewlinesToFirstIndent(CodeStream& cs) {
  for (;;) {
    Token token = nextToken(cs);
    if (token.isIndent()) return token.indentLevel;
    assert(token.newline);
  }
}
