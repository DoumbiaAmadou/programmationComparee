#!/bin/bash

echo "gcl -load pow.lisp -batch" > pow
chmod +x pow
./pow
rm pow
