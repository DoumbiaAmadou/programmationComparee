#!/usr/bin/env bash

function msg () {
  normal='\033[0m'
  echo -e "${1}$2${normal}"
}

function warn () {
  red='\033[0;31m'
  msg $red $1
}

function inform () {
  green='\033[0;32m'
  msg $green $1
}

for i in `find . -maxdepth 1 -mindepth 1 -type d`; do
  cd $i
  echo -n "Checking $i "
  (chmod u+rx ./compile.sh && ./compile.sh) &> compile.log
  if [ ! -e anagram ]; then warn "KO"; else inform "OK"; fi;
  cd .. 
done
    
