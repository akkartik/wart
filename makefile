C=10
wart_bin: file_list prim_func_list test_list transform_list
	@echo `git whatchanged -p -$C |grep "^[+ -][^+-]" |perl -pwe 's/(.).*/$$1/' |uniq |grep "+" |wc -l` hunks added in last $C commits
	g++ -g -Wall -Wextra boot.cc -o wart_bin
	@echo

file_list: *.cc
	@ls *.cc |grep -wv "boot.cc\|test.cc" |perl -pwe 's/.*/#include "$$&"/' > file_list
	@ls *.cc |grep test.cc |perl -pwe 's/.*/#include "$$&"/' >> file_list

prim_func_list: *.cc
	@grep -h "^COMPILE_PRIM_FUNC" *.cc |perl -pwe 's/.*COMPILE_PRIM_FUNC\(([^,]*), ([^,]*), ([^,]*),$$/{ L"$$1", $$3, $$2 },/' > prim_func_list

test_list: *.test.cc
	@grep -h "^[[:space:]]*void test_" *.test.cc |perl -pwe 's/^\s*void (.*)\(\) {$$/$$1,/' > test_list

transform_list: *.cc
	@grep -h "^[[:space:]]*Cell\* transform_" *.cc |perl -pwe 's/^\s*Cell\* (.*)\(.*/$$1,/' > transform_list

clean:
	rm -rf wart_bin* *_list
