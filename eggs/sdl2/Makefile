default: build

build:
	chicken-install -n

install:
	chicken-install

install-test:
	chicken-install -test

install-profile:
	chicken-install -D sdl2-profile

uninstall:
	chicken-uninstall sdl2

test:
	cd tests && csi -s run.scm

clean:
	rm -f *.import.scm *.types *.c *.o *.so
