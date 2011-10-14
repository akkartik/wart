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
