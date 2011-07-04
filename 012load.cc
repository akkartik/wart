// HACK because there's no wifstream(wstring) constructor
// will only work with strings containing ascii characters
vector<ascii> toAscii(string s) {
  vector<ascii> result;
  for (string::iterator p = s.begin(); p != s.end(); ++p)
    result.push_back(*p);
  return result;
}

COMPILE_PRIM_FUNC(load, (f),
  ifstream f(&toAscii(toString(lookup(L"f")))[0]);
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(f))));
  for (list<Cell*>::iterator p = cells.begin(); p != cells.end(); ++p)
    rmref(eval(*p));
)
