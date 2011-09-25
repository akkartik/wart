//// insert explicit parens based on indentation

#define inc(p) { ++p; if (p == end) return p; }
#define pop(l) { l.pop_front(); if (l.empty()) break; }
list<Token>::iterator slurpNextLine(list<Token>& line, list<Token>::iterator p, list<Token>::iterator end) {
  if (!line.empty())
    line.pop_front(); // indent
  while (!line.empty() && !line.front().isIndent())
    pop(line); // tokens

  if (line.empty() && p != end) { // initial condition
    line.push_back(*p); // indent;
    inc(p);
  }

  while (p != end && !p->isIndent()) {
    line.push_back(*p);
    inc(p);
  }
  if (p != end && p->isIndent()) {
    line.push_back(*p);
    inc(p);
  }
  return p;
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

                                  int parenCount = 0;
                                  void add(list<Token>& l, Token x) {
                                    if (!x.isIndent())
                                      l.push_back(x);
                                    if (x == L"(") ++parenCount;
                                    if (x == L")") --parenCount;
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

list<Token> parenthesize(list<Token> in) {
  list<Token> result;
  stack<int> implicitParenStack;
  int suppressInsert = 0;

  list<Token> line;
  Token prevLineIndent=Token::indent(0), thisLineIndent=Token::indent(0), nextLineIndent=Token::indent(0);
  for (list<Token>::iterator p=in.begin(), q=slurpNextLine(line, p, in.end());
        p != in.end();
        p=q, q=slurpNextLine(line, p, in.end())) {
    prevLineIndent=thisLineIndent, thisLineIndent=line.front(), nextLineIndent=line.back();

    bool insertedParenThisLine = false;
    if (!suppressInsert && numWordsInLine(line) > 1
        && !alreadyGrouped(line)
        && !(p != in.begin() && thisLineIndent.indentLevel == prevLineIndent.indentLevel+1 && thisLineIndent.isIndent())) {
      // open paren
      add(result, Token::of(L"("));
      implicitParenStack.push(thisLineIndent.indentLevel);
      insertedParenThisLine = true;
    }

    // copy line tokens
    for (list<Token>::iterator q = line.begin(); q != line.end(); ++q) {
      add(result, *q);

      if (parenNotAtStartOfLine(q, line.begin()))
        suppressInsert = parenCount; // no more paren-insertion until it closes

      if (*q == L")" && parenCount <= suppressInsert) // it closed
        suppressInsert = 0;
    }

    if (suppressInsert) continue;

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
