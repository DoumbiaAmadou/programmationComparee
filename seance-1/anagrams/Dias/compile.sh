#!/bin/sh

if [ -z "$1" ]
then
    echo "Compile"
    scalac Anagrams.scala
elif [ "$1" = "clean" ]
then
    echo "Cleaning"
    rm *.class
fi

echo "Done"
