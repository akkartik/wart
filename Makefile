C=10
a.out: boot.cc prim_func_list test_list
	g++ -g -Wall -Wextra boot.cc
	@echo
	@echo `cat boot.cc |strip_tests.pl |grep -v "^ *//\|^\\s*\?$$" |wc -l` LoC
	@echo `git whatchanged -p -$C |grep "^[+ -][^+-]" |perl -pwe 's/(.).*/$$1/' |uniq |grep "+" |wc -l` hunks added in last $C commits
	@echo assignments: `grep "\->car = " boot.cc |wc -l` car, `grep "\->cdr = " boot.cc |wc -l` cdr

prim_func_list: boot.cc
	@grep "^COMPILE_PRIM_FUNC" boot.cc |perl -pwe 's/COMPILE_PRIM_FUNC\(([^,]*), ([^,]*),$$/{ L"$$1", L"$$2", primFunc_$$1 },/' > prim_func_list

test_list: boot.cc
	@grep -h "^[[:space:]]*void test" boot.cc |perl -pwe 's/^\s*void (.*)\(\) {$$/$$1,/' > test_list

clean:
	rm -rf a.out* prim_func_list test_list
