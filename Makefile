GERBIL_PATH = $(HOME)/.gerbil
LOCAL_LIB = .gerbil/lib/gerbil-aws
GLOBAL_LIB = $(GERBIL_PATH)/lib/gerbil-aws
BINARY = .gerbil/bin/gerbil-aws
OPENSSL_RPATH = /home/linuxbrew/.linuxbrew/opt/openssl@3/lib

.PHONY: build clean install

build:
	gerbil build
	patchelf --set-rpath $(OPENSSL_RPATH) $(BINARY)
	@rm -f $(GLOBAL_LIB)
	ln -sf $(CURDIR)/$(LOCAL_LIB) $(GLOBAL_LIB)

clean:
	gerbil clean
	rm -f $(GLOBAL_LIB)

install:
	sudo cp $(BINARY) /usr/local/bin
