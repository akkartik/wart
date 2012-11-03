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

list<Token> nextExpr(CodeStream& c) {
  list<Token> result;
  long openExplicitParens = 0;  // parens in the original
  stack<long> implicitParenStack;   // parens we inserted

  Token indent = nextToken(c);  // indent token; sets currIndent

  list<Token> line;
  long numWordsInLine = 0;
  bool parenAtStartOfLine = false;
  while (!c.fd.eof()) {
    Token curr = nextToken(c);
    if (curr.newline) {
      if (!line.empty()) {
        for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
          result.push_back(*p);
          if (*p == "(") ++openExplicitParens;
          if (*p == ")") --openExplicitParens;
          if (openExplicitParens < 0) RAISE << "Unbalanced )" << endl;
        }
        line.clear();

        if (implicitParenStack.empty() && openExplicitParens == 0)
          break;
      }
    }
    else if (curr.isQuoteOrUnquote()) {
      if (numWordsInLine < 2)
        line.push_back(curr);
      else
        result.push_back(curr);
    }
    else if (curr.isParen()) {
      if (numWordsInLine < 2) {
        line.push_back(curr);
        if (!parenAtStartOfLine)
          parenAtStartOfLine = (curr == "(" && numWordsInLine == 0);
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
      }
    }
    else if (!curr.isIndent()) { // curr is a 'word' token
      if (numWordsInLine < 1) {
        line.push_back(curr);
      }
      else if (numWordsInLine == 1) {
        if (openExplicitParens == 0 && !parenAtStartOfLine) {
          result.push_back(Token("("));
          implicitParenStack.push(c.currIndent);
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
      ++numWordsInLine;
    }
    else { // curr.isIndent()
      if (!line.empty()) {
        for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
          result.push_back(*p);
          if (*p == "(") ++openExplicitParens;
          if (*p == ")") --openExplicitParens;
          if (openExplicitParens < 0) RAISE << "Unbalanced )" << endl;
        }
        line.clear();
      }

      if (implicitParenStack.empty() && openExplicitParens == 0) {
        if (!c.fd.eof())
          for (int i = 0; i < c.currIndent; ++i)
            c.fd.putback(' ');
        c.atStartOfLine = true;
        break;
      }

      while (!implicitParenStack.empty() && c.currIndent <= implicitParenStack.top()) {
        result.push_back(Token(")"));
        implicitParenStack.pop();
      }

      if (implicitParenStack.empty() && openExplicitParens == 0) {
        if (!c.fd.eof())
          for (int i = 0; i < c.currIndent; ++i)
            c.fd.putback(' ');
        c.atStartOfLine = true;
        break;
      }

      // reset
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
