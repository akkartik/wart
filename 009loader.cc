#undef char
#include<dirent.h>
#define char wchar_t

void loadFile(ascii* filename) {
  ifstream f(filename);
  list<Cell*> cells = buildCells(parse(parenthesize(tokenize(f))));
  for (list<Cell*>::iterator p = cells.begin(); p != cells.end(); ++p)
    rmref(eval(*p));
  for (list<Cell*>::iterator p = cells.begin(); p != cells.end(); ++p)
    rmref(*p);
}

void loadFiles(const ascii* ext) {
  struct dirent *ent;
  DIR* dir = opendir(".");
  while ((ent = readdir(dir)) != NULL) {
    unsigned int n = strlen(ent->d_name), extn = strlen(ext);
    if (n < extn) continue;
    if (strncmp(&ent->d_name[n-extn], ext, extn)) continue;
    if (!isdigit(ent->d_name[0])) continue;
    loadFile(ent->d_name);
  }
  closedir(dir);
}
