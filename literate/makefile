a.out: makefile wart.cc function_list test_list ../[0-9]*.cc
	g++ -g -Wall -Wextra -fno-strict-aliasing wart.cc
	@a.out test

wart.cc: 0* ../wart_bin
	../wart_bin tangle 0* > wart.cc

../wart_bin: ../[0-9]*.cc
	@cd .. && make

function_list: wart.cc
	@grep -h "^[^ #].*) {" wart.cc |perl -pwe 's/ {/;/' > function_list

test_list: wart.cc
	@grep -h "^[[:space:]]*void test_" wart.cc |perl -pwe 's/^\s*void (.*)\(\) {$$/$$1,/' > test_list

clean:
	@cd .. && make clean
	rm -rf wart.cc a.out *_list
