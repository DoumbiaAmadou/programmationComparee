#!/bin/sh

COMP=ocamlc
OPT_COMP=-c
OPT_LINK=-o
ANAGRAM=anagram
CLEAN="rm -f"

compile () {
    $COMP $ANAGRAM.mli
    $COMP $OPT_COMP $ANAGRAM.ml
    $COMP $OPT_LINK $ANAGRAM $ANAGRAM.cmo
}

clean () {
    $CLEAN $ANAGRAM.cmi $ANAGRAM.cmo
}

compile
clean
