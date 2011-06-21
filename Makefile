C=10
a.out: x.cc test_list
	g++ -g -Wall -Wextra x.cc
	@echo
	@echo `cat x.cc |strip_tests.pl |grep -v "^ *//\|^\?$$" |wc -l` LoC
	@echo `git whatchanged -p -$C |grep "^[+ -][^+-]" |perl -pwe 's/(.).*/$$1/' |uniq |grep "+" |wc -l` hunks added in last $C commits

test_list: x.cc
	@grep -h "^\s*void test" *.cc |perl -pwe 's/^\s*void (.*)\(\) {$$/$$1,/' > test_list
