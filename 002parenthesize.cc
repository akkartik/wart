//// insert explicit parens based on indentation

#define inc(p) { ++p; if (p == end) return p; }
#define pop(l) { l.pop_front(); if (l.empty()) break; }
list<Token>::iterator slurpNextLine(list<Token>& currLine, list<Token>::iterator in, list<Token>::iterator end) {
  if (!currLine.empty())
    currLine.pop_front(); // indent
  while (!currLine.empty() && !currLine.front().isIndent())
    pop(currLine); // tokens

  if (in != end && currLine.empty()) { // initial condition
    currLine.push_back(*in); // indent;
    inc(in);
  }

  while (in != end && !in->isIndent()) {
    currLine.push_back(*in);
    inc(in);
  }
  if (in != end && in->isIndent()) {
    currLine.push_back(*in);
    inc(in);
  }
  return in;
}
#undef pop
#undef inc



                                  bool isParen(Token x) {
                                    return x == L"(" || x == L")";
                                  }

                                  bool isQuoteOrUnquote(Token x) {
                                    return x == L"'" || x == L"`" || x == L"," || x == L",@" || x == L"@";
                                  }

                                  ostream& operator<<(ostream& os, list<Token> l) {
                                    bool prevWasOpen = true;
                                    for (list<Token>::iterator p = l.begin(); p != l.end(); ++p) {
                                      if (!p->isIndent() && *p != L")" && !prevWasOpen) os << " ";
                                      prevWasOpen = (*p == L"(" || isQuoteOrUnquote(*p));

                                      if (p->isIndent()) {
                                        os << endl;
                                        for (int i=0; i < p->indentLevel; ++i)
                                          os << " ";
                                      }
                                      else {
                                        os << *p << "("<<p->indentLevel<<")";
                                      }
                                    }
                                    return os << endl;
                                  }

                                  int numWordsInLine(list<Token> line) {
                                    int numWords = 0;
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
                                    if (*begin == L"`") begin++;
                                    if (q == begin) return false;
                                    return (*q == L"(");
                                  }

                                  Token nthTokenInLine(list<Token> line, int n) {
                                    list<Token>::iterator p = line.begin();
                                    for (int i = 0; i < n; ++i)
                                      ++p;
                                    return *p;
                                  }

                                  bool alreadyGrouped(list<Token> line) {
                                    Token firstToken = nthTokenInLine(line, 1);
                                    Token secondToken = nthTokenInLine(line, 2); // line must have 2 tokens
                                    return firstToken == L"("
                                        || (isQuoteOrUnquote(firstToken) && secondToken == L"(");
                                  }

                                  bool continuationLine(Token startOfCurrLine, stack<int> parenStack) {
                                    return !parenStack.empty()
                                        && startOfCurrLine.indentLevel == parenStack.top()+1;
                                  }

list<Token> parenthesize(list<Token> in) {
  list<Token> result;
  stack<int> explicitParenStack; // parens in the original
  stack<int> implicitParenStack; // parens we inserted
  int argParenCount=0; // depth of unprocessed parens not at start of line

  list<Token> line;
  list<Token>::iterator p = slurpNextLine(line, in.begin(), in.end());
  for(; !line.empty(); p=slurpNextLine(line, p, in.end())) {
    Token thisLineIndent=line.front(), nextLineIndent=line.back();

    bool insertedParenThisLine = false;
    if (!argParenCount && numWordsInLine(line) > 1 && !alreadyGrouped(line) && !continuationLine(thisLineIndent, explicitParenStack)) {
      // open paren
      add(result, Token::of(L"("));
      implicitParenStack.push(thisLineIndent.indentLevel);
      insertedParenThisLine = true;
    }

    // copy line tokens
    for (list<Token>::iterator q = line.begin(); q != line.end(); ++q) {
      add(result, *q);

      if (*q == L"(")
        explicitParenStack.push(q->indentLevel);
      if (*q == L")")
        explicitParenStack.pop();

      if (*q == L"(" && (parenNotAtStartOfLine(q, line.begin()) || argParenCount))
        ++argParenCount; // no more paren-insertion until it closes
      if (*q == L")" && argParenCount) // it closed
        --argParenCount;
    }

    if (argParenCount) continue;

    if (nextLineIndent.indentLevel <= thisLineIndent.indentLevel && insertedParenThisLine) {
      // close paren for this line
      add(result, Token::of(L")"));
      implicitParenStack.pop();
    }

    if (nextLineIndent.indentLevel < thisLineIndent.indentLevel)
      while (!implicitParenStack.empty() && implicitParenStack.top() >= nextLineIndent.indentLevel) {
        // close paren for a previous line
        add(result, Token::of(L")"));
        implicitParenStack.pop();
      }
  }

  for (unsigned int i=0; i < implicitParenStack.size(); ++i)
    result.push_back(Token::of(L")"));
  return result;
}
