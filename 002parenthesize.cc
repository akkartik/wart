//// insert explicit parens based on indentation

                                  bool endOfInput(istream& in) {
                                    if (in.eof()) return true;
                                    if (!interactive) return false;
                                    // in interactive mode two <Enter>s are like eof
                                    bool ans = false;
                                    char c = in.get();
                                    if (c == '\n' && !in.eof() && in.peek() == '\n')
                                      ans = true;
                                    in.putback(c);
                                    return ans;
                                  }

list<Token> nextLine(CodeStream& c) {
  list<Token> result;
  if (endOfInput(c.fd)) return result;

  if (c.currIndent == -1)
    result.push_back(Token::indent(c.currIndent=indent(c.fd)));
  else
    result.push_back(Token::indent(c.currIndent));

  do { result.push_back(nextToken(c)); }
  while (!endOfInput(c.fd) && !result.back().isIndent());
  return result;
}



                                  bool isParen(Token x) {
                                    return x == "(" || x == ")";
                                  }

                                  bool isQuoteOrUnquote(Token x) {
                                    return x == "'" || x == "`" || x == "," || x == ",@" || x == "@";
                                  }

                                  long numWordsInLine(list<Token> line) {
                                    long numWords = 0;
                                    for (list<Token>::iterator p = line.begin(); p != line.end(); ++p)
                                      if (!p->isIndent() && !isParen(*p)
                                          && !isQuoteOrUnquote(*p))
                                        ++numWords;
                                    return numWords;
                                  }

                                  void add(list<Token>& l, Token x) {
                                    if (!x.isIndent())
                                      l.push_back(x);
                                  }

                                  bool parenNotAtStartOfLine(list<Token>::iterator q, list<Token>::iterator begin) {
                                    while (begin->isIndent()) begin++;
                                    if (isQuoteOrUnquote(*begin)) begin++;
                                    if (q == begin) return false;
                                    return (*q == "(");
                                  }

                                  Token nthTokenInLine(list<Token> line, long n) {
                                    list<Token>::iterator p = line.begin();
                                    for (long i = 0; i < n; ++i)
                                      ++p;
                                    return *p;
                                  }

                                  bool alreadyGrouped(list<Token> line) {
                                    Token firstToken = nthTokenInLine(line, 1);
                                    Token secondToken = nthTokenInLine(line, 2); // line must have 2 tokens
                                    return firstToken == "("
                                        || (isQuoteOrUnquote(firstToken) && secondToken == "(");
                                  }

                                  bool continuationLine(long currLineIndent, stack<long> parenStack) {
                                    return !parenStack.empty() && currLineIndent == parenStack.top()+1;
                                  }

list<Token> nextExpr(CodeStream& c) {
  list<Token> result;
  stack<long> explicitParenStack; // parens in the original
  stack<long> implicitParenStack; // parens we inserted
  long argParenCount=0; // depth of unprocessed parens not at start of line
  for (list<Token> line = nextLine(c); !line.empty(); line=nextLine(c)) {
    long thisLineIndent=line.front().indentLevel, nextLineIndent=line.back().indentLevel;

    bool insertedParenThisLine = false;
    if (!argParenCount && numWordsInLine(line) > 1 && !alreadyGrouped(line) && !continuationLine(thisLineIndent, explicitParenStack)) {
      // open paren
      add(result, Token::of("("));
      implicitParenStack.push(thisLineIndent);
      insertedParenThisLine = true;
    }

    // copy line tokens
    for (list<Token>::iterator q = line.begin(); q != line.end(); ++q) {
      add(result, *q);

      if (*q == "(")
        explicitParenStack.push(q->indentLevel);
      if (*q == ")") {
        if (explicitParenStack.empty())
          RAISE << "Unbalanced )" << endl << DIE;
        explicitParenStack.pop();
      }

      if (*q == "(" && (parenNotAtStartOfLine(q, line.begin()) || argParenCount))
        ++argParenCount; // no more paren-insertion until it closes
      if (*q == ")" && argParenCount) // it closed
        --argParenCount;
    }

    if (argParenCount) continue;

    if (nextLineIndent <= thisLineIndent && insertedParenThisLine) {
      // close paren for this line
      add(result, Token::of(")"));
      implicitParenStack.pop();
    }

    if (nextLineIndent < thisLineIndent)
      while (!implicitParenStack.empty() && implicitParenStack.top() >= nextLineIndent) {
        // close paren for a previous line
        add(result, Token::of(")"));
        implicitParenStack.pop();
      }

    if (implicitParenStack.empty() && explicitParenStack.empty() && argParenCount == 0)
      break;
  }

  for (unsigned long i=0; i < implicitParenStack.size(); ++i)
    result.push_back(Token::of(")"));
  return result;
}
