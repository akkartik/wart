wart.cc: makefile function_list test_list
	../wart_bin tangle 0* > wart.cc
	g++ -g -Wall -Wextra -fno-strict-aliasing wart.cc

function_list: [0-9]*
	@grep -h "^[^ #].*) {" [0-9]* |perl -pwe 's/ {/;/' > function_list

test_list: [0-9]*
	@grep -h "^[[:space:]]*void test_" [0-9]* |perl -pwe 's/^\s*void (.*)\(\) {$$/$$1,/' > test_list

clean:
	rm -rf wart.cc a.out *_list
