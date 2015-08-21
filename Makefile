.DEFAULT_GOAL := all

######################################################################

#OCAML_SOURCE_DIRS := lib test
#include build-support/OCamlSrcs.makefile

SRCDIR := lib
include build-support/OCamlSrcs.makefile

######################################################################
.PHONY: all clean install uninstall

all: lib/_build/pretty-monospace.cma \
     lib/_build/pretty-monospace.cmxa

# FIXME: test

install: lib/_build/pretty-monospace.cma \
     lib/_build/pretty-monospace.cmxa
	@ocamlfind install pretty-monospace META \
            lib/_build/pretty-monospace.cma \
            lib/_build/pretty-monospace.cmxa \
            lib/_build/pretty-monospace.a \
            lib/_build/*.cmi \
            lib/_build/*.cmx

uninstall:
	@ocamlfind remove pretty-monospace

clean:
	rm -rf $(BUILDDIRS)

