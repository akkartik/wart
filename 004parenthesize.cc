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

  dbg << "--- nextExpr\n";
  dbg << "consuming indent token\n";
  Token indent = nextToken(c);  // indent token; sets currIndent
  dbg << "is it indent? " << indent << endl;
  dbg << "currIndent " << c.currIndent << endl;

  list<Token> line;
  long numWordsInLine = 0;
  bool parenAtStartOfLine = false;
  while (!c.fd.eof()) {
    Token curr = nextToken(c);
    dbg << "token: " << curr << endl;
    if (curr.newline) {
      dbg << "newline\n";
      if (!line.empty()) {
        for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
          dbg << "unbuffering1 " << *p << endl;
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
      if (numWordsInLine < 2) {
        dbg << "buffering1 " << curr << endl;
        line.push_back(curr);
      }
      else {
        dbg << "adding1 " << curr << endl;
        result.push_back(curr);
      }
    }
    else if (curr.isParen()) {
      if (numWordsInLine < 2) {
        dbg << "buffering2 " << curr << endl;
        line.push_back(curr);
        if (!parenAtStartOfLine)
          parenAtStartOfLine = (curr == "(" && numWordsInLine == 0);
      }
      else {
        for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
          dbg << "unbuffering5 " << *p << endl;
          result.push_back(*p);
          if (*p == "(") ++openExplicitParens;
          if (*p == ")") --openExplicitParens;
          if (openExplicitParens < 0) RAISE << "Unbalanced )" << endl;
        }
        line.clear();

        dbg << "adding2 " << curr << endl;
        result.push_back(curr);
        if (curr == "(") ++openExplicitParens;
        if (curr == ")") --openExplicitParens;
        if (openExplicitParens < 0) RAISE << "Unbalanced )" << endl;
      }
    }
    else if (!curr.isIndent()) { // curr is a 'word' token
      dbg << "word " << numWordsInLine << ": " << curr << endl;
      if (numWordsInLine < 1) {
        dbg << "buffering3 " << curr << endl;
        line.push_back(curr);
      }
      else if (numWordsInLine == 1) {
        if (openExplicitParens == 0 && !parenAtStartOfLine) {
          dbg << "inserting implicit (\n";
          result.push_back(Token("("));
          implicitParenStack.push(c.currIndent);
        }

        for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
          dbg << "unbuffering2 " << *p << endl;
          result.push_back(*p);
          if (*p == "(") ++openExplicitParens;
          if (*p == ")") --openExplicitParens;
          if (openExplicitParens < 0) RAISE << "Unbalanced )" << endl;
        }
        line.clear();

        dbg << "adding3 " << curr << endl;
        result.push_back(curr);
      }
      else {
        dbg << "adding4 " << curr << endl;
        result.push_back(curr);
      }
      ++numWordsInLine;
      dbg << curr << " -- words in line now " << numWordsInLine << endl;
    }
    else { // curr.isIndent()
      dbg << "indent now " << c.currIndent << endl;
      if (!line.empty()) {
        for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
          dbg << "unbuffering3 " << *p << endl;
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
        dbg << "inserting implicit )\n";
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

  dbg << "wrapping up\n";
  for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
    dbg << "unbuffering4 " << *p << endl;
    result.push_back(*p);
  }
  line.clear();

  for (unsigned long i=0; i < implicitParenStack.size(); ++i) {
    dbg << "inserting2 implicit )\n";
    result.push_back(Token(")"));
  }
  return result;
}



// internals

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
