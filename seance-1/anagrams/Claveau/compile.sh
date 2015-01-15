#!/bin/bash
javac Element.java
javac Anagramme.java
jar cvmf MANIFEST.MF anagram.jar *.class
