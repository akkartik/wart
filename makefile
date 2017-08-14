wart_bin: makefile type_list function_list file_list test_file_list test_list compiledfn_list
	g++ -O3 -Wall -Wextra -fno-strict-aliasing boot.cc -o wart_bin        # (takes ~15 seconds)

type_list: boot.cc [0-9]*.cc
	@# assumes struct decl has space before '{'
	@grep -h "^struct .* {" [0-9]*.cc |sed 's/\(struct *[^ ]*\).*/\1;/' > type_list
	@grep -h typedef [0-9]*.cc >> type_list

function_list: boot.cc [0-9]*.cc
	@# assumes function decl has space before '{'
	@grep -h "^[^ #].*) {" [0-9]*.cc |sed 's/ {.*/;/' > function_list
	@grep -h "^COMPILE_FN" [0-9]*.cc |sed 's/.*COMPILE_FN(\([^,]*\), \([^,]*\), \([^,]*\),$$/cell* \2();/' >> function_list

file_list: boot.cc [0-9]*.cc
	@ls [0-9]*.cc |grep -v "\.test\.cc$$" |sed 's/.*/#include "&"/' > file_list

test_file_list: [0-9]*.test.cc
	@ls [0-9]*.test.cc |sed 's/.*/#include "&"/' > test_file_list

test_list: [0-9]*.cc
	@grep -h "^[[:space:]]*void test_" [0-9]*.cc |sed 's/^\s*void \(.*\)() {$$/\1,/' > test_list

compiledfn_list: [0-9]*.cc
	@grep -h "^COMPILE_FN" [0-9]*.cc |sed 's/.*COMPILE_FN(\([^,]*\), \([^,]*\), \([^,]*\),$$/{ "\1", \3, \2 },/' > compiledfn_list

clean:
	rm -rf wart_bin* *_list
