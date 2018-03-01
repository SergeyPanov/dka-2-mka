COMPILER=ghc
GFLAGS=--make
ERR=./automates/err/
OUTPUTS=./automates/outputs/
TEST=test.sh
SHELL=bash

compile:
	$(COMPILER) $(GFLAGS) dka-2-mka.hs

clean:
	rm -rf *.hi *.o dka-2-mka $(ERR) $(OUTPUTS)

test: compile
	$(SHELL) $(TEST)

