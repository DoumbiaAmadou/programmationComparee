<?php

include_once('BIblio.php');
include_once('play.php');

/*
This function create or join a party
Read the configuration file, the first line contains the value "create" or "join".
If hte first value is "create", the second line of the file may contains all the parameters to configure the party.
If hte first value is "join", the second line of the file may contains the id of the game to join.
In the "join" case, if the second line is empty, so the function search an accessible party between the last 15 ones.
*/

function startGame(){

	auth("testSS" , "s123456s");

	$file = dirname(__FILE__).'/conf.txt';
	$game_id = null;
	$join_tentative = null;

	if (file_exists($file)) {

		$instr = file($file);
		
		if (substr($instr[0],0,6)=="create"){
		
			echo "Create game<br/>";
		
			$params = $instr[1];
			$params = str_replace("\"","",$params);
			$params = explode(",",$params);

			if(sizeof($params)!=9){
				echo "wrong number of parameters in instructions file<br/>";
				return null;
			} 

			$game_creation = create(  $params[0],
								$params[1],
								$params[2],
								$params[3],
								$params[4],
								$params[5],
								$params[6],
								$params[7],
								$params[8]
							);

			$game_id = $game_creation->response->identifier;
			$join_tentative =joinGame($game_id);
		}

		else if (substr($instr[0],0,4)=="join"){
		
			echo "Join game<br/>";
			
			if(sizeof($instr)>1)
				$game_id = $instr[1];
			
			else{
				$games_list =games();
				$i = sizeof($games_list->response->games);

				$found = false;

				while($i>sizeof($games_list->response->games)-15 && $found==false){
					$game_id = $games_list->response->games[$i-1]->game_description->identifier;
					$game_status = status($game_id);
					
					if ($game_status->response->status->status->status=="initialization" ||
						$game_status->response->status->status->status=="playing"){
						$found = true;
					}
					$i--;
				}

				if ($i==sizeof($games_list->response->games)-15){
					echo "failed to found a party.<br/>";
					return null;
				}	
			}

			$join_tentative = joinGame($game_id);
		}

		if($join_tentative!=null)
			if($join_tentative->status=="error"){
				echo "cannot join the game: ".$join_tentative->response->error_msg."<br/>";
				return null;
			}

	}

	return $game_id;
}

$game_id = startGame();

if($game_id !=null) play($game_id);

?>