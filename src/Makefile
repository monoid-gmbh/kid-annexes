.PHONY: pkgs clean

all: Category2.c Category3.c

pkgs:
	futhark pkg add github.com/diku-dk/cpprandom
	futhark pkg add github.com/diku-dk/sorts
	futhark pkg add github.com/diku-dk/statistics
	futhark pkg sync

libs: libkidcat2.so libkidcat3.so

release: libs
	tar zcvf kid-annexes.tar.gz libkidcat*.so *.c *.h

Category2.c: Prelude.fut Base.fut Category2.fut
	futhark multicore --library Category2.fut

Category3.c: Prelude.fut Base.fut Category3.fut
	futhark multicore --library Category3.fut

libkidcat2.so: Category2.c
	gcc Category2.c -o libkidcat2.so -fPIC -shared

libkidcat3.so: Category3.c
	gcc Category3.c -o libkidcat3.so -fPIC -shared

clean:
	rm -f *.c *.h *.py *.o *.so
