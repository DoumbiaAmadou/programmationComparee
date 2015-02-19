#!/bin/bash

if [[ -e "anagram.lisp" ]]; then
    clisp -c --quiet anagram.lisp > /dev/null
    echo "#!/usr/bin/clisp" | cat - anagram.fas > anagram
    chmod +x anagram 
else
    echo "anagram.lisp doesn't exist"
fi
