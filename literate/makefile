a.out: makefile wart.cc function_list test_list
	g++ -g -Wall -Wextra -fno-strict-aliasing wart.cc

wart.cc: 0*
	../wart_bin tangle 0* > wart.cc

function_list: wart.cc
	@grep -h "^[^ #].*) {" wart.cc |perl -pwe 's/ {/;/' > function_list

test_list: wart.cc
	@grep -h "^[[:space:]]*void test_" wart.cc |perl -pwe 's/^\s*void (.*)\(\) {$$/$$1,/' > test_list

clean:
	rm -rf wart.cc a.out *_list
