// http://stackoverflow.com/questions/772355/how-to-inherit-from-stdostream/5966638#5966638
#include<iostream>
using std::ostream;

class LoggedStream {
public:
  LoggedStream(ostream& _out):out(_out){}
  template<typename T>
  const LoggedStream& operator<<(const T& v) const {log();out << v;return *this;}
protected:
  void log() const { std::cerr << "Printing" << std::endl;}
  ostream& out;
};

main() {LoggedStream(std::cout) << "log" << "Three" << "times";}
