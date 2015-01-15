#!/usr/bin/env bash

. bench-utils.sh

for i in `find . -maxdepth 1 -mindepth 1 -type d`; do
  cd $i
  echo -n "Checking $i "
  (chmod u+rx ./compile.sh && ./compile.sh) &> compile.log
  if [ ! -e anagram ]; then warn "KO"; else inform "OK"; fi;
  cd ..
done
echo -n "Checking ./Régis-Gianas "; warn "KO"
