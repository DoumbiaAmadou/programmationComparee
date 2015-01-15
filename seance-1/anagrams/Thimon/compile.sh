#!/bin/bash
javac Anagram.java
cp Anagram.java anagram
java Anagram "niche" "truc" "marie" "cabans"
rm Anagram.class
rm *~
