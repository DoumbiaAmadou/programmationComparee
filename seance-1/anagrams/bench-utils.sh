
function msg () {
  normal='\033[0m'
  echo -e "${1}$2${normal}"
}

function warn () {
  red='\033[0;31m'
  msg $red $1
}

function inform () {
  green='\033[0;32m'
  msg $green $1
}
