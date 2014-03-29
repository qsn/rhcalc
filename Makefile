.PHONY: all clean check

all: rhcalc

rhcalc: check
	ghc --make rhcalc

clean:
	rm -f *.o *.hi *.o-boot *.hi-boot *\~

check:
	@echo -n "Looking for readline... "
	@((ghc-pkg list | grep readline > /dev/null) && echo "found") || echo "not found"
