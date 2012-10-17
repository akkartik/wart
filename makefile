wart_bin: type_list function_list file_list test_file_list test_list compiled_fn_list
	g++     -std=gnu++0x -Wall -Wextra -fno-strict-aliasing boot.cc -o wart_bin

opt:  		type_list function_list file_list test_file_list test_list compiled_fn_list
	g++ -O3 -std=gnu++0x -Wall -Wextra -fno-strict-aliasing boot.cc -o wart_bin

type_list: *.cc
	@grep -h "^struct .*{$$" *.cc |perl -pwe 's/(struct *[^ ]*).*/$$1;/' > type_list
	@grep -h typedef *.cc >> type_list

function_list: *.cc
	@grep -h "^[^ ].*) {$$" *.cc |perl -pwe 's/ {/;/' > function_list
	@grep -h "^COMPILE_FN" [0-9]*.cc |perl -pwe 's/.*COMPILE_FN\(([^,]*), ([^,]*), ([^,]*),$$/Cell* $$2();/' >> function_list

file_list: *.cc
	@ls [0-9]*.cc |grep -v "\.test\.cc$$" |perl -pwe 's/.*/#include "$$&"/' > file_list

test_file_list: *.cc
	@ls [0-9]*.test.cc |perl -pwe 's/.*/#include "$$&"/' > test_file_list

test_list: *.test.cc
	@grep -h "^[[:space:]]*void test_" [0-9]*.test.cc |perl -pwe 's/^\s*void (.*)\(\) {$$/$$1,/' > test_list

compiled_fn_list: *.cc
	@grep -h "^COMPILE_FN" [0-9]*.cc |perl -pwe 's/.*COMPILE_FN\(([^,]*), ([^,]*), ([^,]*),$$/{ "$$1", $$3, $$2 },/' > compiled_fn_list

clean:
	rm -rf wart_bin* *_list

.PHONY: opt, clean
