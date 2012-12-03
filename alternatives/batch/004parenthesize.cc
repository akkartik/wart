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
  long numWordsInLine = 0;
  bool parenAtStartOfLine = false;
  list<Token> buffer;
  for (list<Token>::iterator p = in.begin(); p != in.end(); ++p) {
    Token curr = *p;
    if (isQuoteOrUnquote(curr)) {
      if (numWordsInLine < 2)
        buffer.push_back(curr);
      else
        emit(curr, result, explicitOpenParens);
    }
    else if (isParen(curr)) {
      if (!parenAtStartOfLine)
        parenAtStartOfLine = (curr == "(" && numWordsInLine == 0);
      if (numWordsInLine < 2 && explicitOpenParens == 0 && !parenAtStartOfLine) {
        buffer.push_back(curr);
      }
      else {
        emitAll(buffer, result, explicitOpenParens);
        emit(curr, result, explicitOpenParens);
      }
    }
    else if (!isIndent(curr)) {   //// 'word' token
      ++numWordsInLine;
      if (numWordsInLine < 2) {
        buffer.push_back(curr);
      }
      else {
        if (numWordsInLine == 2 && explicitOpenParens == 0 && !parenAtStartOfLine) {
          result.push_back(Token("("));
          implicitOpenParens.push(curr.indentLevel);
        }
        emitAll(buffer, result, explicitOpenParens);
        emit(curr, result, explicitOpenParens);
      }
    }
    else {  //// indent
      emitAll(buffer, result, explicitOpenParens);
      while (!implicitOpenParens.empty() && curr.indentLevel <= implicitOpenParens.top()) {
        result.push_back(Token(")"));
        implicitOpenParens.pop();
      }

      //// reset
      numWordsInLine = 0;
      parenAtStartOfLine = false;
    }
  }

  emitAll(buffer, result, explicitOpenParens);
  for (unsigned long i=0; i < implicitOpenParens.size(); ++i)
    result.push_back(Token(")"));
  return result;
}



//// internals

void emit(Token& t, list<Token>& out, long& explicitOpenParens) {
  out.push_back(t);
  if (t == "(") ++explicitOpenParens;
  if (t == ")") --explicitOpenParens;
  if (explicitOpenParens < 0) RAISE << "Unbalanced )" << endl;
}

void emitAll(list<Token>& buffer, list<Token>& out, long& explicitOpenParens) {
  for (list<Token>::iterator p = buffer.begin(); p != buffer.end(); ++p)
    emit(*p, out, explicitOpenParens);
  buffer.clear();
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
