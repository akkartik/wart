wart_bin: file_list transform_list compiled_fn_list test_list test_file_list
	g++ -g -Wall -Wextra boot.cc -o wart_bin
	@echo

file_list: *.cc
	@ls *.cc |grep -wv "boot.cc\|test.cc" |perl -pwe 's/.*/#include "$$&"/' > file_list

test_file_list: *.cc
	@ls *.cc |grep test.cc |perl -pwe 's/.*/#include "$$&"/' > test_file_list

compiled_fn_list: *.cc
	@grep -h "^COMPILE_FN" *.cc |perl -pwe 's/.*COMPILE_FN\(([^,]*), ([^,]*), ([^,]*),$$/{ "$$1", $$3, $$2 },/' > compiled_fn_list

test_list: *.test.cc
	@grep -h "^[[:space:]]*void test_" *.test.cc |perl -pwe 's/^\s*void (.*)\(\) {$$/$$1,/' > test_list

transform_list: *.cc
	@grep -h "^[[:space:]]*Cell\* transform_" *.cc |perl -pwe 's/^\s*Cell\* (.*)\(.*/$$1,/' > transform_list

clean:
	rm -rf wart_bin* *_list
