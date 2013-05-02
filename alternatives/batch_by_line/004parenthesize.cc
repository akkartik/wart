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

list<Token> insertImplicitParens(list<Token> in) {
  list<Token> result;   // emit tokens here
  long explicitOpenParens = 0;  // parens in the original
  stack<long> implicitOpenParens;   // parens we inserted

  for (list<Token> line = nextLine(in); !line.empty(); line=nextLine(in)) {
    long thisLineIndent=line.front().indentLevel, nextLineIndent=line.back().indentLevel;

    // open an implicit paren if necessary
    if (explicitOpenParens == 0 && numWordsInLine(line) > 1 && noParenAtStart(line)) {
      result.push_back(Token("("));
      implicitOpenParens.push(thisLineIndent);
    }

    // copy line tokens
    for (list<Token>::iterator q = line.begin(); q != line.end(); ++q) {
      if (!isIndent(*q))
        result.push_back(*q);
      if (*q == "(") ++explicitOpenParens;
      if (*q == ")") --explicitOpenParens;
      if (explicitOpenParens < 0)
        RAISE << "Unbalanced )" << endl;
    }

    // close all possible implicit parens
    while (!implicitOpenParens.empty() && implicitOpenParens.top() >= nextLineIndent) {
      result.push_back(Token(")"));
      implicitOpenParens.pop();
    }
  }

  for (unsigned long i=0; i < implicitOpenParens.size(); ++i)
    result.push_back(Token(")"));
  return result;
}



//// internals

list<Token> nextLine(list<Token>& in) {
  list<Token> result;
  if (in.empty()) return result;

  // this line's indent
  result.push_back(nextToken(in));

  while (!in.empty() && !isIndent(in.front()))
    result.push_back(nextToken(in));

  // next line's indent
  if (!in.empty())
    result.push_back(in.front());

  return result;
}

Token nextToken(list<Token>& in) {
  Token result = in.front(); in.pop_front();
  return result;
}

long numWordsInLine(list<Token> line) {
  long numWords = 0;
  for (list<Token>::iterator p = line.begin(); p != line.end(); ++p)
    if (!isIndent(*p) && !isParen(*p) && !isQuoteOrUnquote(*p))
      ++numWords;
  return numWords;
}

list<Token>::iterator firstNonQuote(list<Token>& line) {
  for (list<Token>::iterator p = line.begin(); p != line.end(); ++p) {
    if (!isIndent(*p) && !isQuoteOrUnquote(*p))
      return p;
  }
  return line.end();
}

bool noParenAtStart(list<Token> line) {
  list<Token>::iterator p = firstNonQuote(line);
  return p != line.end() && *p != "(";
}

bool isIndent(const Token& t) {
  return t.token == "";
}

bool isParen(const Token& t) {
  return t == "(" || t == ")";
}

bool isQuoteOrUnquote(const Token& t) {
  return t == "'" || t == "`"
      || t == "," || t == ",@" || t == "@";
}
