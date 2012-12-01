// C++ style:
//  no pointers except Cell*
//  use long as the default integer type; it's always large enough to hold pointers

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
#include<tr1/unordered_map>
using std::tr1::unordered_map;
#include<tr1/unordered_set>
using std::tr1::unordered_set;
#include<algorithm>

#include<string>
using std::string;

#include<iostream>
using std::istream;
using std::ostream;
using std::iostream;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

#include<sstream>
using std::stringstream;
using std::ostringstream;

#include<fstream>
using std::ifstream;
using std::ofstream;



long debug = 0;
#define dbg if(debug == 1) cerr
#define dbg2 if(debug == 2) cerr

#define unused __attribute__((unused))

bool pretendRaise = false;
long raiseCount = 0;

                           // ?: to avoid dangling-else warnings
#define RAISE pretendRaise ? ++raiseCount,cerr \
                           : cerr << __FILE__ << ":" << __LINE__ << " "

struct Die {};
ostream& operator<<(ostream& os, unused Die die) {
  os << "dying";
  exit(1);
}
Die DIE;



// interpreter decls

#include "type_list"

#include "function_list"

// interpreter impl

#include "file_list"

// interpreter tests

#include "test_file_list"
