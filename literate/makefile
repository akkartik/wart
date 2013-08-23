a.out: makefile wart.cc function_list test_list primfn_list ../[0-9]*.cc
	g++ -g -Wall -Wextra -fno-strict-aliasing wart.cc
	-@a.out test

# To see what the program looks like after all layers have been applied, read
# wart.cc
wart.cc: 0* ../wart_bin
	./tangle 0* > wart.cc

../wart_bin: ../[0-9]*.cc
	@echo Building tangle
	@cd .. && make >/dev/null

# auto-generated files; by convention they end in '_list'.
function_list: wart.cc
	@grep -h "^[^ #].*) {" wart.cc |perl -pwe 's/ {.*/;/' > function_list
	@grep -h "^COMPILE_FN" wart.cc |perl -pwe 's/.*COMPILE_FN\(([^,]*), ([^,]*), ([^,]*),$$/cell* $$2();/' >> function_list

test_list: wart.cc
	@grep -h "^[[:space:]]*void test_" wart.cc |perl -pwe 's/^\s*void (.*)\(\) {.*/$$1,/' > test_list

primfn_list: wart.cc
	@grep -h "^COMPILE_FN" wart.cc |perl -pwe 's/.*COMPILE_FN\(([^,]*), ([^,]*), ([^,]*),$$/{ "$$1", $$3, $$2 },/' > primfn_list

clean:
	@cd .. && make clean
	rm -rf wart.cc a.out *_list
