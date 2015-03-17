# all :
# 	ocamlbuild -use-ocamlfind -pkgs curl,yojson,netclient,str main.native 

# clean :
# 	ocamlbuild -clean
# 	rm *.*~

# FILES = IO.ml
# FLAGS =
# CAMLC = ocamlfind ocamlc $(FLAGS) $(LIB)
# CAMLOPT = ocamlfind ocamlopt $(FLAGS) $(LIB)
# CAMLDEP = ocamlfind ocamldep $(LIB)
# LIB= -package curl,yojson,netclient
# OBJS=$(FILES:.ml=.cmo)
# MLIS=$(FILES:.ml=.mli)
# PROG=foo

# all: depend $(PROG)

# $(PROG): $(OBJS)
# 	$(CAMLOPT) -o $(PROG) $(OBJS)

# .SUFFIXES: .ml .mli .cmo .cmi .cmx

# .ml.cmo: 
# 	$(CAMLC) -c $<

# .mli.cmi:
# 	$(CAMLC) -c $<

# .ml.cmx:
# 	$(CAMLOPT) -c $<

# depend:
# 	$(CAMLDEP) $(MLIS) $(FILES) > .depend 

# clean:
# 	rm -f $(PROG) *.cm[ioxa] *~

########################## User's variables #####################
#
# The Caml sources (including camlyacc and camllex source files)

SOURCES = IO.ml main.ml

# The executable file to generate

EXEC = antroid

########################## Advanced user's variables #####################
#
# The Caml compilers.
# You may fix here the path to access the Caml compiler on your machine
# You may also have to add various -I options.
CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLFIND = ocamlfind

all: $(EXEC)

opt : $(EXEC).opt

FLAGS = -linkpkg
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)
PACKAGE = -package curl -package yojson -package netclient -package str

$(EXEC): $(OBJS)
	$(CAMLFIND) $(CAMLC) $(PACKAGE) -o $(EXEC) $(LIBS) $(OBJS) $(FLAGS)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLFIND) $(CAMLOPT) $(PACKAGE) -o $(EXEC) $(LIBS:.cma=.cmxa) $(OPTOBJS) $(FLAGS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx 

.ml.cmo:
	$(CAMLFIND) $(CAMLC) $(PACKAGE) -c $< $(FLAGS)

.mli.cmi:
	$(CAMLFIND) $(CAMLC) $(PACKAGE) -c $< $(FLAGS)

.ml.cmx:
	$(CAMLFIND) $(CAMLOPT) $(PACKAGE) -c $< $(FLAGS)
clean:
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(EXEC)
	rm -f $(EXEC).opt

depend: $(SOURCES2)
	$(CAMLFIND) $(CAMLDEP) *.mli *.ml > .depend

include .depend
