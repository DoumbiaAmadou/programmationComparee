#!/bin/bash


echo "gcl -load helloworld.lisp -batch" > helloworld
chmod +x helloworld
./helloworld
rm helloworld
