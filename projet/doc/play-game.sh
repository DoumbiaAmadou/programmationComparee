#!/usr/bin/env bash

NBTURNS=100
NBANTS=5
PACE=10
E0=1
A0=100

if [ ! -e jq ]; then
  curl -k http://stedolan.github.io/jq/download/linux64/jq
  chmod u+x jq
fi

OUT=''
ant () {
  FILTER=$1
  shift 1
  echo '% ./antroid.sh ' "$@"
  echo -n '=> '
  if [ x$FILTER == x"raw" ]; then
      ./antroid.sh "$@"
  else
      OUT=`./antroid.sh "$@" | tee log | ./jq $FILTER | tr -d \"`
      if [ $? -ne 0 ]; then cat log; fi
      echo $OUT
  fi
}

ant '.status' register a f
ant '.status' auth a f
ant '.response.identifier' new 'a' 'Dance floor for hacking ants!' $PACE $NBTURNS $NBANTS 1 1 $E0 $A0
GID=$OUT
ant '.status' join $GID
TURN=0

ants_do () {
    A=0
    CMDS=""
    while [ $A -ne $NBANTS ]; do
      CMDS=$A:$1,$CMDS
      A=$(($A + 1))
    done
    ant '.response' play $GID $CMDS
}

while [ $TURN -ne $NBTURNS ]; do
    TURN=$(($TURN + 7))
    ants_do "left"
    ants_do "forward"
    ants_do "forward"
    ants_do "right"
    ants_do "attack@1"
    ants_do "rest"
    ants_do "hack@[L1>left;jump!L1;]"
    ant '.status.score' join $GID
done

ant raw log