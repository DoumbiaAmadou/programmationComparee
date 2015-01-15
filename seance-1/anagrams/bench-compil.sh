#!/usr/bin/env bash

#TESTS="coussin écran plainte ragent police aimer couille romain"

. bench-utils.sh

for i in `find . -maxdepth 1 -mindepth 1 -type d`; do
  cd $i
  echo -n "Checking $i "
  chmod u+rx ./compile.sh
  TIME="$( /usr/bin/time -f '%e' ./compile.sh 2>&1 1>/dev/null )"
  if [ ! -e anagram ]; 
  then warn "KO"; 
  else inform "OK"; 
      msg "Compilation time: ${TIME}"; 
  fi;
  cd ..
done

