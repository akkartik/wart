COMPILE_FN(load, compiledfn_load, "($f)",
  load_file(to_string(lookup("$f")).c_str());
  return nil;
)

void load_files(const char* ext) {
  vector<char*> files = sorted_files(".", ext);
  for (vector<char*>::iterator p = files.begin(); p != files.end(); ++p)
    load_file(*p);
}



//// internals

void load_file(const char* filename) {
  bool old_interactive = Interactive; Interactive = false;
  ifstream f(filename);
  if (f.fail()) return;
  while (!f.eof()) {
    TEMP(form, read(f));
    trace("dump ") << form << '\n';  // remove trailing space to track down errors in wart files
    rmref(eval(form));
  }
  Interactive = old_interactive;
}

#include<dirent.h>

vector<char*> sorted_files(const char* dirname, const char* ext) {
  vector<char*> result;
  dirent** files;
  int num_files = scandir(dirname, &files, NULL, alphasort);
  for (int i = 0; i < num_files; ++i) {
    unsigned long n = strlen(files[i]->d_name), extn = strlen(ext);
    if (n < extn) continue;
    if (strncmp(&files[i]->d_name[n-extn], ext, extn)) continue;
    if (!isdigit(files[i]->d_name[0])) continue;
    char* s = new char[n+1];  // leak
    strncpy(s, files[i]->d_name, n+1);
    result.push_back(s);
  }
  return result;
}
