list<string> lines(istream& in) {
  list<string> result;
  while (!in.eof()) {
    string s;
    getline(in, s);
    result.push_back(s);
  }
  return result;
}

struct Hunk {
  string filename;
  int starting_line;
  list<string> lines;
  Hunk() :starting_line(0) {}
};

list<Hunk> hunks(istream& in) {
  list<Hunk> result;
  list<string> allLines = lines(in);
  result.push_back(Hunk());
  result.back().starting_line = 1;
  result.back().lines.swap(allLines);
  return result;
}
