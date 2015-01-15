#! /bin/bash

. bench-utils.sh

__sort_line() {
  cut -d: -f2 $1 | xargs | tr ' ' '\n' | sort | tr '\n' ' ' | xargs
}

__test_word() {
  local word=$1
  local expected="$2"
  local out=.out-$RANDOM
  local output=

  echo -n "- execution with '$word': "
  ./anagram "$word" > $out 2>/dev/null
  if [ "$?" -ne "0" ]; then
    warn "KO"
    rm -f $out
    return 1
  else
    output=$(__sort_line $out)
    if [ "$output" != "$expected" ]; then
      warn "KO"
      echo " Expected:          '$expected'"
      echo " Normalized Output: '$output'"
      rm -f $out
      return 1
    else
      inform "OK"
    fi
  fi

  rm -f $out
}

__time_dir() {
    cd $1
    echo "Checking $1 "

    echo -n "- compilation: "
    (chmod u+rx ./compile.sh && ./compile.sh) &> compile.log
    if [ ! -e anagram ]; then warn "KO"; cd ..; continue; else inform "OK"; fi

    __test_word "coussin" "cosinus cousins coussin cuisons cuisson sucions" && \
    __test_word "yolo" "" && \
    __test_word "a" "a"

    cd ..
}

__time() {
  local dirs=

  if [ "$#" -gt 0 ]; then
    dirs=$@
  else
    dirs=`find . -maxdepth 1 -mindepth 1 -type d`
  fi

  for d in $dirs; do
    __time_dir $d
  done
}

__time $*
