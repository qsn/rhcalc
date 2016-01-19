SRC=Core.hs Core.hs-boot Main.hs Makefile Operators.hs Parser.hs Stack.hs

.PHONY: all clean check

all: rhcalc

rhcalc: check $(SRC)
	ghc --make -o rhcalc Main

clean:
	rm -f *.o *.hi *.o-boot *.hi-boot *\~

check:
	@echo -n "Looking for readline... "
	@((ghc-pkg list | grep -q '\sreadline') && echo "found") || echo "not found"
	@echo -n "Looking for parsec... "
	@((ghc-pkg list | grep -q '\sparsec') && echo "found") || echo "not found"
	@echo -n "Looking for mtl... "
	@((ghc-pkg list | grep -q '\smtl') && echo "found") || echo "not found"
