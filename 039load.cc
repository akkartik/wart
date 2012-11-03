#include<dirent.h>

COMPILE_FN(load, compiledFn_load, "($f)",
  loadFile(toString(lookup("$f")).c_str());
  return nil;
)

void loadFiles(const char* ext) {
  vector<char*> files = sortedFiles(".", ext);
  for (vector<char*>::iterator p = files.begin(); p != files.end(); ++p)
    loadFile(*p);
}



// internals

void loadFile(const char* filename) {
  ifstream f(filename);
  if (f.fail()) return;
  while (!f.eof()) {
    Cell* cell = read(f);
//?     cerr << cell << endl;   // uncomment this to track down errors in wart files
    rmref(eval(cell));
    rmref(cell);
  }
}

vector<char*> sortedFiles(const char* dirname, const char* ext) {
  vector<char*> result;
  dirent** files;
  int numFiles = scandir(dirname, &files, NULL, alphasort);
  for (int i = 0; i < numFiles; ++i) {
    unsigned int n = strlen(files[i]->d_name), extn = strlen(ext);
    if (n < extn) continue;
    if (strncmp(&files[i]->d_name[n-extn], ext, extn)) continue;
    if (!isdigit(files[i]->d_name[0])) continue;
    char* s = new char[n+1];  // leak
    strncpy(s, files[i]->d_name, n+1);
    result.push_back(s);
  }
  return result;
}
