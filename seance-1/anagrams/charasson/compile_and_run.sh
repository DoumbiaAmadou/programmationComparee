#!/bin/sh
mkdir bin
cd bin 
echo "compilation du fichier"
fsc ../src/Anagram.scala
echo "execution du fichier"
scala Anagram
