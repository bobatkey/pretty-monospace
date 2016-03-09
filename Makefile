.DEFAULT_GOAL := all

######################################################################

SRCDIR := lib
include build-support/OCamlSrcs.makefile

SRCDIR := test
include build-support/OCamlSrcs.makefile

######################################################################
.PHONY: all clean install uninstall

all: lib/_build/pretty-monospace.cma \
     lib/_build/pretty-monospace.cmxa

doc: doc/index.html doc/style.css

doc/index.html: lib/Pretty.mli
	mkdir -p doc
	ocamldoc -html -d doc -css-style style.css lib/Pretty.mli

doc/style.css: style.css
	cp $< $@

test: test/_build/native_bin/test
	$<

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
	rm -rf doc

