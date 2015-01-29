#!/bin/bash
javac Anagram.java
cp Anagram.java anagram
java Anagram "niche" "truc" "marie" "yolo" "a"
rm Anagram.class
rm *~
