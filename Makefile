OCB_FLAGS = -use-ocamlfind -I src -I test
OCB       = ocamlbuild $(OCB_FLAGS)


test:	native byte

clean:
	$(OCB) -clean

native:	
	$(OCB) horner.native
	mv horner.native test

byte:	
	$(OCB) horner.byte
	mv horner.byte test

doc:
	$(OCB) fex.docdir/index.html

.PHONY:
	all clean byte native doc
