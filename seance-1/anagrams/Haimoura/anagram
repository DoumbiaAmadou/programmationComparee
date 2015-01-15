#!/usr/bin/php
<?php
	$words = file("../words");
	for ($i=1; $i<sizeof($argv) ; $i++) { 
		$chars = str_split($argv[$i]);
		sort($chars);
		echo $argv[$i]." : ";
		foreach ($words as $word) {
			$word = trim($word);
		 	$chars2 = str_split($word);
		 	sort($chars2);
		 	if ($chars === $chars2) {
		 		echo "$word ";
		 	}
		 }
		 echo "\n"; 
	}
?>