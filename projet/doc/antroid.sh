#!/usr/bin/env bash

API_VERSION_NUMBER=0
#ANTROID=https://localhost/antroid/$API_VERSION_NUMBER
ANTROID=https://yann.regis-gianas.org/antroid/$API_VERSION_NUMBER
COOKIES=.cookies
CURL="curl -s -k --cookie $COOKIES --cookie-jar $COOKIES"

function error {
    echo $1
    exit 1
}

if [ $# -lt 1 ]; then
    error "antroid.sh command options"
fi

register () {
    if [ $# -ne 3 ]; then
        error "antroid.sh register login password"
    fi
    $CURL -X POST $ANTROID/register -d login=$2 -d password=$3
}

auth () {
    if [ $# -ne 3 ]; then
        error "antroid.sh auth login password"
    fi
    $CURL $ANTROID/auth -X POST -d login=$2 -d password=$3
}

new () {
    if [ $# -ne 10 ]; then
        error "antroid.sh new users teaser pace nb_turn nb_ant_per_player nb_player minimal_nb_player initial_energy initial_acid"
    fi
    $CURL -G $ANTROID/create                    \
       -d users="${2}"                          \
       --data-urlencode "teaser=${3}"           \
       -d pace="${4}"                           \
       -d nb_turn="${5}"                        \
       -d nb_ant_per_player="${6}"              \
       -d nb_player="${7}"                      \
       -d minimal_nb_player="${8}"              \
       -d initial_energy="${9}"                 \
       -d initial_acid="${10}"
}

status () {
    if [ $# -ne 2 ]; then
        error "antroid.sh status game_id"
    fi
    $CURL -G $ANTROID/status -d id=$2
}

join () {
    if [ $# -ne 2 ]; then
        error "antroid.sh join game_id"
    fi
    $CURL -G $ANTROID/join -d id=$2
}

destroy () {
    if [ $# -ne 2 ]; then
        error "antroid.sh destroy game_id"
    fi
    $CURL -G $ANTROID/destroy -d id=$2
}

play () {
    if [ $# -ne 3 ]; then
        error "antroid.sh play game_id k:[left|right|forward|backward|attack:N|put:N]"
    fi
    $CURL -G $ANTROID/play -d id=$2 -d cmds=$3
}

observe () {
    if [ $# -ne 3 ]; then
        error "antroid.sh observe game_id ant_id"
    fi
    $CURL -G $ANTROID/observe -d id=$2 -d antid=$3
}

log () {
   if [ $# -ne 2 ]; then
        error "antroid.sh log game_id"
    fi
    $CURL -G $ANTROID/log -d id=$2
}

atomic () {
    if [ $# -ne 1 ]; then
        error "antroid.sh $1"
    fi
    $CURL $ANTROID/$1
}

case $1 in
    register) register "$@";;
    auth) auth "$@" ;;
    new) new "$@" ;;
    status) status "$@" ;;
    join) join "$@" ;;
    log) log "$@" ;;
    destroy) destroy "$@" ;;
    play) play "$@" ;;
    observe) observe "$@" ;;
    api|whoami|logout|games) atomic "$@" ;;
    *) echo "Unknown command."
esac