#!/usr/bin/env bash

#TESTS="coussin écran plainte ragent police aimer couille romain"

. bench-utils.sh

for i in `find . -maxdepth 1 -mindepth 1 -type d`; do
  cd $i
  echo -n "Checking $i "
  N_BEFORE="$( find ./ -type f | wc -l )"
  STRING="$(du -sh ../$i)"
  S_BEFORE="${STRING:0:$( expr index "${STRING}" ' .' )-2}"
  chmod u+rx ./compile.sh
  TIME="$( /usr/bin/time -f '%e' ./compile.sh 2>&1 1>/dev/null )"
  if [ ! -e anagram ]; 
  then warn "KO"; 
  else inform "OK"; 
      N_AFTER="$( find ./ -type f | wc -l )"
      STRING="$(du -sh ../$i)"
      S_AFTER="${STRING:0:$( expr index "${STRING}" ' .' )-2}"
      N_NEW=$((N_AFTER-N_BEFORE))
#      S_NEW=$((S_AFTER-S_BEFORE))
      echo "Before: $S_BEFORE"
      msg "Compilation time: ${TIME}, new files: ${N_NEW}, size before: ${S_BEFORE}, size after: ${S_AFTER}"; 
  fi;
  cd ..
done

