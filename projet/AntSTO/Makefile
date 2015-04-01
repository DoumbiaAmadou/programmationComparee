OCAMLBUILDFLAGS:=-I src -pkg curl -pkg yojson -docflags -stars,-charset,utf8

all:
	ocamlbuild $(OCAMLBUILDFLAGS) Programm.native
clean:
	ocamlbuild -clean
doc:
	ocamlbuild $(OCAMLBUILDFLAGS) main.docdir/index.html
