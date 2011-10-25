#undef char
#include<dirent.h>
#define char wchar_t

void loadFile(ascii* filename) {
  ifstream f(filename);
  list<Cell*> cells = wartRead(f);
  for (list<Cell*>::iterator p = cells.begin(); p != cells.end(); ++p)
    rmref(eval(*p));
  for (list<Cell*>::iterator p = cells.begin(); p != cells.end(); ++p)
    rmref(*p);
}

                                  vector<ascii*> sortedFiles(const ascii* dirname, const ascii* ext) {
                                    vector<ascii*> result;
                                    dirent** files;
                                    int numFiles = scandir(dirname, &files, NULL, alphasort);
                                    for (int i = 0; i < numFiles; ++i) {
                                      unsigned int n = strlen(files[i]->d_name), extn = strlen(ext);
                                      if (n < extn) continue;
                                      if (strncmp(&files[i]->d_name[n-extn], ext, extn)) continue;
                                      if (!isdigit(files[i]->d_name[0])) continue;
                                      ascii* s = new ascii[n+1]; // leak
                                      strncpy(s, files[i]->d_name, n+1);
                                      result.push_back(s);
                                    }
                                    return result;
                                  }

void loadFiles(const ascii* ext) {
  vector<ascii*> files = sortedFiles(".", ext);
  for (vector<ascii*>::iterator p = files.begin(); p != files.end(); ++p)
    loadFile(*p);
}



                                  // HACK because there's no wifstream(wstring) constructor
                                  // will only work with strings containing ascii characters
                                  vector<ascii> toAscii(string s) {
                                    vector<ascii> result;
                                    for (string::iterator p = s.begin(); p != s.end(); ++p)
                                      result.push_back(*p);
                                    return result;
                                  }

COMPILE_PRIM_FUNC(load, primFunc_load, L"($f)",
  loadFile(&toAscii(toString(lookup(L"$f")))[0]);
  return nil;
)
