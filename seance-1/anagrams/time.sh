#! /bin/bash

. bench-utils.sh

__test_word() {
  local word=$1
  local out=.out-$RANDOM

  echo -n "- execution with '$word': "
  ./anagram "$word" > $out 2>/dev/null
  if [ "$?" -ne "0" ]; then
    warn "KO"
    rm -f $out
    return 1
  else
    grep -q "^$word: " $out
    if [ "$?" -ne "0" ]; then
      warn "KO"
      rm -f $out
      return 1
    else
      inform "OK"
      echo -n "  ==> "
      cat $out
    fi
  fi

  rm -f $out
}

__time() {

  for i in `find . -maxdepth 1 -mindepth 1 -type d`; do
    cd $i
    echo "Checking $i "

    echo -n "- compilation: "
    (chmod u+rx ./compile.sh && ./compile.sh) &> compile.log
    if [ ! -e anagram ]; then warn "KO"; cd ..; continue; else inform "OK"; fi

    __test_word "coussin" && \
    __test_word "yolo" && \
    __test_word "a"

    cd ..
  done

}

__time $*
