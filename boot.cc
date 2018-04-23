// C++ style:
//  no pointers except cell*
//  use long as the default integer type; it's always large enough to hold pointers

#define vestigial __attribute__((unused))

#include<cstdio>
#include<cstring>
#include<cstdlib>
#include<errno.h>
#include<time.h>
#include<math.h>
#include<vector>
using std::vector;
#include<list>
using std::list;
#include<stack>
using std::stack;
#include<set>
using std::set;
#include<map>
using std::map;
#include<utility>
using std::pair;

#include<algorithm>

#include<string>
using std::string;
const size_t NOT_FOUND = string::npos;

#include<iostream>
using std::istream;
using std::ostream;
using std::iostream;
using std::cin;
using std::cout;
using std::cerr;

#include<sstream>
using std::stringstream;
using std::istringstream;
using std::ostringstream;

#include<fstream>
using std::ifstream;
using std::ofstream;



// interpreter decls

#include "type_list"

#include "function_list"

// interpreter impl

#include "file_list"

// interpreter tests

#include "test_file_list"
