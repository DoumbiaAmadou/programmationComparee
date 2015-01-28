#!/bin/bash
javac Anagram.java
jar -cvmf MANIFEST.MF anagram Anagram.class
chmod +x anagram
rm Anagram.class
