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
PACKAGES= -package curl -package yojson -package str
JS_PACKAGE=-package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o
JS_OF_CAML= js_of_ocaml
FLAGS = -linkpkg

all: $(EXEC).js

opt : $(EXEC).opt

$(EXEC).js: $(EXEC).byte
	$(JS_OF_CAML) $(EXEC).byte
	mv $(EXEC).js ./www/js/

$(EXEC).byte: $(MLIS) $(OBJS) 
	$(CAMLFIND) $(CAMLC) $(JS_PACKAGES) $(PACKAGES) -o $(EXEC).byte $(LIBS) $(OBJS) $(FLAGS)

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
	rm -f *.o


