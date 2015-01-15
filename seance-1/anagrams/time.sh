#! /bin/bash

. bench-utils.sh

__time() {

  local prefix=$RANDOM
  local output=

  for i in `find . -maxdepth 1 -mindepth 1 -type d`; do
    cd $i
    echo "Checking $i "

    echo -n "- compilation: "
    (chmod u+rx ./compile.sh && ./compile.sh) &> compile.log
    if [ ! -e anagram ]; then warn "KO"; cd ..; continue; else inform "OK"; fi

    echo -n "- execution with 'coussin': "
    ./anagram coussin > .${prefix}stdout 2>/dev/null
    if [ "$?" -ne "0" ]; then
      warn "KO"
    else
      grep -q "^coussin: " .${prefix}stdout
      if [ "$?" -ne "0" ]; then
        warn "KO"
      else
        inform "OK"
        echo -n "  ==> "
        cat .${prefix}stdout

        # TODO time
      fi
    fi
    rm -f .${prefix}stdout

    cd ..
  done

}

__time $*
