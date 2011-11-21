#include<dirent.h>

void loadFile(const char* filename) {
  ifstream f(filename);
  CodeStream c(f);
  while (!eof(c.fd)) {
    Cell* cell = read(c);
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
                                      char* s = new char[n+1]; // leak
                                      strncpy(s, files[i]->d_name, n+1);
                                      result.push_back(s);
                                    }
                                    return result;
                                  }

void loadFiles(const char* ext) {
  vector<char*> files = sortedFiles(".", ext);
  for (vector<char*>::iterator p = files.begin(); p != files.end(); ++p)
    loadFile(*p);
}



COMPILE_PRIM_FUNC(load, primFunc_load, "($f)",
  loadFile(toString(lookup("$f")).c_str());
  return nil;
)
