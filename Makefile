BINARY = .gerbil/bin/gerbil-aws
OPENSSL_RPATH = /home/linuxbrew/.linuxbrew/opt/openssl@3/lib

.PHONY: build clean

build:
	gerbil build
	patchelf --set-rpath $(OPENSSL_RPATH) $(BINARY)

clean:
	gerbil clean

install:
	cp .gerbil/bin/gerbil-aws /usr/local/bin
