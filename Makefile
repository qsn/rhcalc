.PHONY: all clean check

all: rhcalc

rhcalc: check
	ghc --make rhcalc

clean:
	rm -f *.o *.hi *.o-boot *.hi-boot *\~

check:
	@echo -n "Looking for readline... "
	@((ghc-pkg list | grep -q '\sreadline') && echo "found") || echo "not found"
	@echo -n "Looking for parsec... "
	@((ghc-pkg list | grep -q '\sparsec') && echo "found") || echo "not found"
	@echo -n "Looking for mtl... "
	@((ghc-pkg list | grep -q '\smtl') && echo "found") || echo "not found"
