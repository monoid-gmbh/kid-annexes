.PHONY: pkgs clean

all: Category2.c Category3.c

Category2.c: Prelude.fut Base.fut Category2.fut
	futhark c --library Category2.fut

Category3.c: Prelude.fut Base.fut Category3.fut
	futhark c --library Category3.fut

pkgs:
	futhark pkg add github.com/diku-dk/cpprandom
	futhark pkg add github.com/diku-dk/sorts
	futhark pkg add github.com/diku-dk/statistics
	futhark pkg sync

clean:
	rm -f *.c *.h *.py *.o *.so
