//// insert explicit parens based on indentation

#define inc(p) { ++p; if (p == end) return p; }
#define pop(l) { l.pop_front(); if (l.empty()) break; }
list<Token>::iterator slurpNextLine(list<Token>& line, list<Token>::iterator p, list<Token>::iterator end) {
  while (!line.empty() && whitespace(line.front().type)) pop(line);
  while (!line.empty() && !whitespace(line.front().type)) pop(line);

  if (line.empty()) // initial condition
    while (p != end) {
      if (!whitespace(p->type)) break;
      line.push_back(*p);
      inc(p);
    }

  while (p != end) {
    if (whitespace(p->type)) break;
    line.push_back(*p);
    inc(p);
  }
  while (p != end) {
    if (!whitespace(p->type)) break;
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
                                    return x == L"'" || x == L"`" || x == L"," || x == L",@";
                                  }

                                  bool isComment(Token x) {
                                    return !whitespace(x.type) && x.token[0] == L';';
                                  }

                                  bool isIndent(Token t) {
                                    return whitespace(t.type) && t != START_OF_LINE;
                                  }

                                  ostream& operator<<(ostream& os, list<Token> l) {
                                    bool prevWasOpen = true;
                                    for (list<Token>::iterator p = l.begin(); p != l.end(); ++p) {
                                      if (*p != L")" && !prevWasOpen) os << " ";
                                      prevWasOpen = (*p == L"(" || isQuoteOrUnquote(*p));

                                      if (*p == START_OF_LINE || isComment(*p))
                                        os << endl;
                                      else if (isIndent(*p))
                                        for (int i=0; i < p->indentLevel; ++i)
                                          os << " ";
                                      else
                                        os << *p;
                                    }
                                    return os << endl;
                                  }

                                  int numWordsInLine(list<Token> line) {
                                    int numWords = 0;
                                    for (list<Token>::iterator p = line.begin(); p != line.end(); ++p)
                                      if (!whitespace(p->type) && !isParen(*p)
                                          && !isComment(*p) && !isQuoteOrUnquote(*p))
                                        ++numWords;
                                    return numWords;
                                  }

                                  Token indentBefore(list<Token> line) {
                                    line.pop_front(); // there's always a START_OF_LINE
                                    if (isIndent(line.front())) return line.front();
                                    return Token::sol();
                                  }

                                  Token indentAfter(list<Token> line) {
                                    if (isIndent(line.back())) return line.back();
                                    if (line.back() == START_OF_LINE) return line.back();
                                    return Token::sol();
                                  }

                                  int parenCount = 0;
                                  void add(list<Token>& l, Token x) {
                                    if (!whitespace(x.type))
                                      l.push_back(x);
                                    if (x == L"(") ++parenCount;
                                    if (x == L")") --parenCount;
                                  }

                                  bool parenNotAtStartOfLine(list<Token>::iterator q, list<Token>::iterator begin) {
                                    while (whitespace(begin->type)) begin++;
                                    if (*begin == L"`") begin++;
                                    if (q == begin) return false;
                                    return (*q == L"(");
                                  }

                                  Token nthTokenInLine(list<Token> line, int n) {
                                    int i = 0;
                                    for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
                                      if (!whitespace(p->type))
                                        ++i;
                                      if (i == n)
                                        return *p;
                                    }
                                    return Token::sol();
                                  }

                                  bool alreadyGrouped(list<Token> line) {
                                    Token firstToken = nthTokenInLine(line, 1);
                                    Token secondToken = nthTokenInLine(line, 2); // line must have 2 tokens
                                    return firstToken == L"("
                                        || (isQuoteOrUnquote(firstToken) && secondToken == L"(");
                                  }

list<Token> parenthesize(list<Token> in) {
  list<Token> result;
  stack<int> parenStack;
  int suppressInsert = 0;

  list<Token> line;
  Token prevLineIndent=Token::sol(), thisLineIndent=Token::sol(), nextLineIndent=Token::sol();
  for (list<Token>::iterator p = in.begin(), q = slurpNextLine(line, p, in.end());
        p != in.end();
        p = q, q = slurpNextLine(line, p, in.end())) {
    prevLineIndent=thisLineIndent, thisLineIndent=indentBefore(line), nextLineIndent=indentAfter(line);

    bool insertedParenThisLine = false;
    if (!suppressInsert && numWordsInLine(line) > 1
        && !alreadyGrouped(line)
        && !(thisLineIndent.indentLevel == prevLineIndent.indentLevel+1 && thisLineIndent.type == MAYBE_WRAP)) {
      // open paren
      add(result, Token::of(L"("));
      parenStack.push(thisLineIndent.indentLevel);
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

    if (nextLineIndent != INDENT /*TODO: MAYBE_WRAP?*/ && insertedParenThisLine) {
      // close paren for this line
      add(result, Token::of(L")"));
      parenStack.pop();
    }

    while (nextLineIndent == OUTDENT
        && !parenStack.empty() && parenStack.top() >= nextLineIndent.indentLevel) {
      // close paren for a previous line
      add(result, Token::of(L")"));
      parenStack.pop();
    }
  }

  for (unsigned int i=0; i < parenStack.size(); ++i)
    result.push_back(Token::of(L")"));
  return result;
}
