#!/bin/bash
export LANG=C.UTF-8
red='\033[0;31m'
green='\033[0;32m'
NC='\033[0m' # No Color

for i in  `find ../ -mindepth 1 -type d | sort `
do 
cd $i
	echo -e "$green \nChecking ${i^^}  $NC "
	if [  -e compile.sh ] ; then 
		chmod u+rx ./compile.sh 
	 	./compile.sh >/tmp/log.log s
		if [  -e anagram  ] 
		then 
			chmod u+rx ./anagram 
			t=0 
			START=0  
			END=0  
			START=$(date +%s.%N)
			for j in $(seq 1 10) 
			do 	
				{
					./anagram abaissassiez abaissassions abaissés abaisseur abaisseurs abaissez abaissiez abaissions abaissons abajoue abajoues abandon abandonna  aube curve > /tmp/log.log 
				} || {
					echo -e "$red\n${i^^} Erreur d'execution : Performance IMPOSSIBLE $NC";START=0 ;break
				}   
			done 
			END=$(date +%s.%N)
			t=$( echo "$END-$START" | bc -l)
			t=$( echo "$t/10" | bc -l) 
			echo  -e  "$green\nPerformance : ${i^^}  =  moyenne $t seconde(s) $NC"
		else 
			echo -e "\n ${i^^} : $red KO impossible compilation  echouée $NC " 
		fi
	else  
		echo -e "$red\n${i^^}  : KO compile.sh n'existe pas $NC " 
	fi
cd - 1>/tmp/log.log
done
