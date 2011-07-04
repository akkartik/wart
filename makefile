C=10
wart_bin: boot_list op_list prim_func_list test_list
	@echo `git whatchanged -p -$C |grep "^[+ -][^+-]" |perl -pwe 's/(.).*/$$1/' |uniq |grep "+" |wc -l` hunks added in last $C commits
	g++ -g -Wall -Wextra boot.cc -o wart_bin
	@echo

prim_func_list: *.cc
	@grep -h "^COMPILE_PRIM_FUNC" *.cc |perl -pwe 's/.*COMPILE_PRIM_FUNC\(([^,]*), ([^,]*),$$/{ L"$$1", L"$$2", primFunc_$$1 },/' > prim_func_list

test_list: *.test.cc
	@grep -h "^[[:space:]]*void test" *.test.cc |perl -pwe 's/^\s*void (.*)\(\) {$$/$$1,/' > test_list

boot_list: *.cc
	@ls *.cc |grep .boot |perl -pwe 's/^/#include "/' |perl -pwe 's/$$/"/' > boot_list

op_list: *.cc
	@ls *.cc |grep -v boot |perl -pwe 's/^/#include "/' |perl -pwe 's/$$/"/' > op_list

clean:
	rm -rf wart_bin* *_list
