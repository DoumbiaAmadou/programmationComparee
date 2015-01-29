#! /bin/bash

__fontaine_compile() {
  local targets=*.c

  for target in $targets; do
    make ${target%%.c}
  done

}

__fontaine_compile $*
