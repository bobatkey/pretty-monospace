.DEFAULT_GOAL := all

######################################################################
.PHONY: all clean install uninstall always

always:

all: _build/lib/pretty-monospace.cma \
     _build/lib/pretty-monospace.cmxa

_build/%: always
	ocamlbuild -use-ocamlfind $*

doc: doc/index.html doc/style.css

doc/index.html: lib/Pretty.mli
	mkdir -p doc
	ocamldoc -html -d doc -css-style style.css -short-functors -colorize-code lib/Pretty.mli

doc/style.css: style.css
	cp $< $@

test: _build/test/test.native
	$<

install: all
	@ocamlfind install pretty-monospace META \
            _build/lib/pretty-monospace.cma \
            _build/lib/pretty-monospace.cmxa \
            _build/lib/pretty-monospace.a \
            _build/lib/*.cmi \
            _build/lib/*.cmx \
            _build/lib/*.cmt \
            _build/lib/*.cmti \
            _build/lib/*.mli

uninstall:
	@ocamlfind remove pretty-monospace

clean:
	rm -rf _build
	rm -rf doc
	rm -f test.native
