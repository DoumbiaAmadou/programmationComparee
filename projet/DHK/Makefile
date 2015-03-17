#*********************************************************************#
#                                                                     #
#                           Objective Caml                            #
#                                                                     #
#            Pierre Weis, projet Cristal, INRIA Rocquencourt          #
#                                                                     #
#  Copyright 1998 Institut National de Recherche en Informatique et   #
#  en Automatique.  Distributed only by permission.                   #
#                                                                     #
#*********************************************************************#


SOURCES = IO.mli IO.ml data.ml AI.ml program.ml main.ml
EXEC = antroit
CAMLFIND = ocamlfind
CAMLC = ocamlc
CAMLOPT = ocamlopt
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)
MLIS = $(SOURCES:.mli=.cmi)
PACKAGES= -package curl -package yojson -package netclient -package str
FLAGS = -linkpkg

all: $(EXEC)

opt : $(EXEC).opt

$(EXEC): $(MLIS) $(OBJS) 
	$(CAMLFIND) $(CAMLC) $(PACKAGES) -o $(EXEC) $(LIBS) $(OBJS) $(FLAGS)

$(EXEC).opt: $(MLIS) $(OPTOBJS) 
	$(CAMLFIND) $(CAMLOPT) $(PACKAGES) -o $(EXEC) $(LIBS:.cma=.cmxa) $(OPTOBJS) $(FLAGS)

.SUFFIXES: 
.SUFFIXES: .ml .mli .cmo .cmi .cmx 

.ml.cmo:
	$(CAMLFIND) $(CAMLC) $(PACKAGES) -c $< $(FLAGS)

.mli.cmi:
	$(CAMLFIND) $(CAMLC) $(PACKAGES) -c $< $(FLAGS)

.ml.cmx:
	$(CAMLFIND) $(CAMLOPT) $(PACKAGES) -c $< $(FLAGS)

clean:
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(EXEC)
	rm -f $(EXEC).opt


