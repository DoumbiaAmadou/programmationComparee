#!/bin/bash
javac Anagram.java
cp Anagram.java anagram
java anagram "niche" "truc" "marie" "cabans"
rm Anagram.class
rm *~
