#!/bin/bash
gcc -Wall archeosortprime.c -o archeosortprime
gcc -Wall gerontosortprime.c -o gerontosortprime
ocamlopt neosortprime.ml -o neosortprime