#!/bin/bash

../bench-utils.sh

$time =0 ; 
$rep = pwd ; 
for i in  `find ../  -mindepth 1 -type d `
do 
cd $i
	echo -n "Checking $i "
	chmod u+rx ./compile.sh 
	./compile.sh 
	if [  -e anagram ]; then 
		for i in $(seq 1 2 20) 
		do 
			START=$(date +%s.%N)
			./anagram abaissassiez abaissassions abaissés abaisseur abaisseurs abaissez abaissiez abaissions abaissons abajoue abajoues abandon abandonna  aube curve 
			END=$(date +%s.%N)
			$time= $time + $(echo "$END - $START" | bc)
			
		done 
		$time = time/20
		inform  "moyenne $time"
	else  
		echo "impossible compilation "; fi;

  cd -
done
