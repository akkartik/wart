C=10
a.out: prim_func_list test_list
	@echo `cat *.cc |strip_tests.pl |grep -v "^ *//\|^\\s*\?$$" |wc -l` LoC
	@echo `git whatchanged -p -$C |grep "^[+ -][^+-]" |perl -pwe 's/(.).*/$$1/' |uniq |grep "+" |wc -l` hunks added in last $C commits
	@echo assignments: `grep "\->car = " boot.cc |wc -l` car, `grep "\->cdr = " boot.cc |wc -l` cdr
	@echo
	g++ -g -Wall -Wextra boot.cc

prim_func_list: comp.cc
	@grep "^COMPILE_PRIM_FUNC" comp.cc |perl -pwe 's/COMPILE_PRIM_FUNC\(([^,]*), ([^,]*), ([^,]*),$$/{ L"$$1", L"$$3", primFunc_$$2 },/' > prim_func_list

test_list: boot.cc comp.cc
	@grep -h "^[[:space:]]*void test" *.cc |perl -pwe 's/^\s*void (.*)\(\) {$$/$$1,/' > test_list

clean:
	rm -rf a.out* prim_func_list test_list
