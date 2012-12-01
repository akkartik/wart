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

list<Token> nextExpr(IndentSensitiveStream& c) {
  list<Token> result;
  long openExplicitParens = 0;  // parens in the original
  stack<long> implicitParenStack;   // parens we inserted

  for (list<Token> line = nextLine(c); !line.empty(); line=nextLine(c)) {
    long thisLineIndent=line.front().indentLevel, nextLineIndent=line.back().indentLevel;

    // open an implicit paren if necessary
    if (openExplicitParens == 0 && numWordsInLine(line) > 1 && noParenAtStart(line)) {
      add(result, Token("("));
      implicitParenStack.push(thisLineIndent);
    }

    // copy line tokens
    for (list<Token>::iterator q = line.begin(); q != line.end(); ++q) {
      add(result, *q);
      if (*q == "(") ++openExplicitParens;
      if (*q == ")") --openExplicitParens;
      if (openExplicitParens < 0)
        RAISE << "Unbalanced )" << endl;
    }

    // close all possible implicit parens
    while (!implicitParenStack.empty() && implicitParenStack.top() >= nextLineIndent) {
      add(result, Token(")"));
      implicitParenStack.pop();
    }

    if (c.fd.eof()) break;

    if (implicitParenStack.empty() && openExplicitParens == 0) {
      if (!c.fd.eof())
        // clean up indent state for the next call
        for (int i = 0; i < nextLineIndent; ++i)
          c.fd.putback(' ');
      break;
    }
  }

  for (unsigned long i=0; i < implicitParenStack.size(); ++i)
    result.push_back(Token(")"));
  return result;
}



// internals

list<Token> nextLine(IndentSensitiveStream& c) {
  list<Token> result;
  if (c.fd.eof()) return result;

  if (c.currIndent == -1)
    result.push_back(nextToken(c));
  else
    result.push_back(Token(c.currIndent));

  do { result.push_back(nextToken(c)); }
  while (!c.fd.eof() && !result.back().isIndent());
  return result;
}

long numWordsInLine(list<Token> line) {
  long numWords = 0;
  for (list<Token>::iterator p = line.begin(); p != line.end(); ++p)
    if (!p->isIndent() && !p->newline && !p->isParen() && !p->isQuoteOrUnquote())
      ++numWords;
  return numWords;
}

void add(list<Token>& l, Token x) {
  if (!x.isIndent() && !x.newline)
    l.push_back(x);
}

list<Token>::iterator firstNonQuote(list<Token>& line) {
  for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
    if (!p->isIndent() && !p->isQuoteOrUnquote())
      return p;
  }
  return line.end();
}

bool noParenAtStart(list<Token> line) {
  list<Token>::iterator p = firstNonQuote(line);
  return p != line.end() && *p != "(";
}
