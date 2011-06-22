C=10
a.out: x.cc test_list
	g++ -g -Wall -Wextra x.cc
	@echo
	@echo `cat x.cc |strip_tests.pl |grep -v "^ *//\|^\\s*\?$$" |wc -l` LoC
	@echo `git whatchanged -p -$C |grep "^[+ -][^+-]" |perl -pwe 's/(.).*/$$1/' |uniq |grep "+" |wc -l` hunks added in last $C commits
	@echo assignments: `grep "\->car = " x.cc |wc -l` car, `grep "\->cdr = " x.cc |wc -l` cdr

test_list: x.cc
	@grep -h "^[[:space:]]*void test" *.cc |perl -pwe 's/^\s*void (.*)\(\) {$$/$$1,/' > test_list

clean:
	rm -rf a.out* test_list
