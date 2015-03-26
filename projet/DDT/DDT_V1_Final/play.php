<?php 
include_once('BIblio.php');

/*
code  de test de la bibliothèque: jouer une partie :
*/
function play($id){

	$st = status($id) ; 
	$sleeptime = 60/$st->response->status->pace ; 
	while($st->response->status->status->status=="initialization") 
	{
		$st = status($id);
		sleep($sleeptime) ; 
	}

	echo "la partie commence<br/>";
	$i=0;
	sleep($sleeptime/2) ;
	
	/*
	boucle de jeu
	*/

	$ant_nb = $st->response->status->nb_ant_per_player;

	$cmd ="0:rest";
	for($num=1;$num<$ant_nb;$num++){
		$cmd=$cmd.",".$num.":rest";
	}

	while($st->response->status->status->status=="playing"){
	
		$resultat = playGame($id, $cmd);
		echo $resultat;
		$i++;
		if(isset ($resultat->response->turn)) 
			$cmd =analyse($resultat) ;
		set_time_limit(500);
		sleep($sleeptime/2) ;
		$i++;
		$st = status($id);
	}
}

function  analyse($resultat){
	/*
	analyse and take a decision
	*/	
	$move =array("rest","left" ,  "right" ,"forward" ) ; 
	$fight = array("attack@5", "hack@");
	$inst = array("JUMP") ;  
	$id = rand(0,sizeof($move)) ;
	echo"<br> echo le resultat <br>";
	print_r($resultat->response);
	$cmd = "";
	for($num=0;$num<sizeof($resultat);$num++){
		$dx =$resultat->response->observations[$num][0]->dx; 
		$dy =$resultat->response->observations[$num][0]->dy; 
		$choix =false; 
		for($i=1 ; $i<sizeof($resultat->response->observations[$num][1]) ; $i++){

			/***
			* if there are no water in front of the ant
			*/
	
			echo"<br><br>"; 
			echo "$i \n";
			print_r($resultat->response->observations[$num][1][$i]);
			echo"<br><br>"; 
			if ( 
				$resultat->response->observations[$num][1][$i]->content->kind=="grass"  
				&& $resultat->response->observations[$num][1][$i]->x==$resultat->response->observations[$num][0]->x + $dx 
				&& $resultat->response->observations[$num][1][$i]->y==$resultat->response->observations[$num][0]->y + $dy )
					$choix =true ;  
	
		}

		if($choix==true)
			$cmd =   $cmd.",".$num.":".$move[3]; 
		else  
			$cmd =   $cmd.",".$num.":".$move[rand(1,2)]; 
	}
	return substr(0,strlen($cmd),$cmd); 
}

function doHackcode($Tactique){
	if($Tactique=="attack"){
	$primitive = array("ant_see" , "" , "",""); 
	}else 
	{
		$primitive = array("ant_see" , "" , "",""); 
	}
	rand(0 , sizeof($primitive)) ; 
}
?>