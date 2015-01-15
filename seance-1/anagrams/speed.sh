#!/bin/bash

. bench-utils.sh


for i in `find . -maxdepth 1 -mindepth 1 -type d`; do
  cd $i
  echo -n "Checking $i "
  (chmod u+rx ./compile.sh && ./compile.sh) &> compile.log
  if [ ! -e anagram ]; then warn "KO"
  else inform "OK"
       echo -e "\033[0;34mLauching anagram of ${i}\033[0m"
       t="Time in seconds : %e s"
       get_time="head -0"
       cmd="./anagram ironique écran aube soigneur cuvé argent Tanger Chicane"
       /usr/bin/time -f "${t}" ${cmd} | head -0 
  fi
  cd ..
done
