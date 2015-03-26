<?php 
include('BIblio.php') ; 
/*
code  de test de la bibliothèque: jouer une nouvelle partie :
*/
 ?>
<html>
<head>
<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
<script>
function sleep(time){
	sleep(time);
}

</script>

</head>
<body>

<?php
$auth = "api";
$l = "test";
$mp = "test";
$url  = "http://http://yann.regis-gianas.org/antroid/0/auth" ; 
auth("amadou12" , "doumbia12");

$retcrea = create("all" , "DDT game" , 10 , 6 , 1 ,1 ,1,10,10); 
//echo"<br> return create  :  <br>" ; 
//print_r($retcrea) ; 

$r =games();
$i = sizeof($r->response->games ); 
$id = $r->response->games[$i-1]->game_description->identifier ; 

$ret = joinGame($id ); 
print_r($ret);
//"295377163279399263610491421675671759" ; 
$st = status($id) ; 
print_r($st->response->status->pace) ; 
//var_dump($st);




echo "  =>  <br>";
//print_r(gettype($st->response->status->pace))  ; 

print_r($st->response->status->turn) ;

//$ste = dolog("854927805303702988149600388713876543") ; 
//print_r($st); 
//
//observe ($id); 

$sleeptime = 60/$st->response->status->pace ; 
echo"<br> sleep time = ".$sleeptime ; 
echo "<br/>";
while($st->response->status->status->status=="initialization") 
{
	$st = status($id);
	echo "en attente<br/>";
	sleep($sleeptime) ; 
}
echo "la partie commence<br/>";

$i=0;
sleep($sleeptime/2) ;
/*
boucle de jeu
*/

while($st->response->status->status->status=="playing" && $i<5){
	
	$cmd ="0:rest" ;

	$resultat = play($id, $cmd);
	//echo ($resultat->response->turn);
	
	echo "tour ".$resultat->response->turn."<br/>";
	//echo "type ".gettype($resultat['response']['observation'])."<br/>";
	//analyse($resultat) ;
	set_time_limit(500);
	sleep($sleeptime/2) ;
	$i++;
	$st = status($id);
}


?>
</body>
</html>
