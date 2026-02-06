GERBIL_PATH = $(HOME)/.gerbil
LOCAL_LIB = .gerbil/lib/gerbil-aws
LOCAL_STATIC = .gerbil/lib/static
GLOBAL_LIB = $(GERBIL_PATH)/lib/gerbil-aws
GLOBAL_STATIC = $(GERBIL_PATH)/lib/static
BINARY = .gerbil/bin/gerbil-aws
OPENSSL_RPATH = /home/linuxbrew/.linuxbrew/opt/openssl@3/lib

.PHONY: build clean install

build:
	gerbil build
	patchelf --set-rpath $(OPENSSL_RPATH) $(BINARY)
	@rm -f $(GLOBAL_LIB)
	ln -sf $(CURDIR)/$(LOCAL_LIB) $(GLOBAL_LIB)
	@mkdir -p $(GLOBAL_STATIC)
	@for f in $(CURDIR)/$(LOCAL_STATIC)/gerbil-aws__*; do ln -sf "$$f" $(GLOBAL_STATIC)/; done

clean:
	gerbil clean
	rm -f $(GLOBAL_LIB)
	rm -f $(GLOBAL_STATIC)/gerbil-aws__*

install: build
	sudo cp $(BINARY) /usr/local/bin
