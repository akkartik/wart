// HACK because there's no wifstream(wstring) constructor
// will only work with strings containing ascii characters
vector<ascii> toAscii(string s) {
  vector<ascii> result;
  for (string::iterator p = s.begin(); p != s.end(); ++p)
    result.push_back(*p);
  return result;
}

COMPILE_PRIM_FUNC(load, L"(f)",
  loadFile(&toAscii(toString(lookup(L"f")))[0]);
)
