#!/bin/bash

white='\033[0m'
red='\033[0;31m'
green='\033[0;32m'

for i in `find ./../ -maxdepth 1 -mindepth 1 -type d`;
do
    
    echo "$i"
    if [ ! -f $i/compile.sh ]; then
    echo -e "${red}pas de compile.sh${white}\n\n"
     else
     	cd $i
     	echo "compile..."
	    ./compile.sh
	    cd - > ./log
      if [ ! -e $i/anagram ]; then
   	        echo -e "${red}pas d'executable anagram${white}\n\n"
	    else
   	        echo -e "${green}OK${white}\n\n"
	    fi
    fi

done
