<?php 
include('BIblio.php') ; 
/****
*
*	index of the preview version
*
*
*
*
*/
 ?>
<html>
<head>
<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
<script>

function callplay(id ,wait){
	location.replace("localhost/badin/Pcomp/waitGame.php?id="+id+"&wait="+wait); 
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

$retcrea = create("all" , "DDT game" , 10 , 5 , 10 ,1 ,1,10,10); 
//echo"<br> return create  :  <br>" ; 
//print_r($retcrea) ; 

$r =games();
$i = sizeof($r->response->games ); 
$id = $r->response->games[$i-1]->game_description->identifier ; 

$ret = joinGame($id ); 
//print_r($ret);
//"295377163279399263610491421675671759" ; 
$st = status($id) ; 
//print_r($st->response->status->pace) ; 
//var_dump($st);

echo"<div id =\"id\">$id</div>";
/*
echo"<div id =\"sleepTime\">$sleeptime = 60/$st->response->status->pace</div>";
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

	sleep($sleeptime) ; 
}

while($st->response->status->status->status=="playing"){
	
	
	
	$cmd ="rest" ; 

	$resultat =play("854927805303702988149600388713876543", $cmd); 
	//analyse($resultat) ; 

}
*/

?>

</body>
<footer onload="callplay(<?php $a = 60/$st->response->status->pace ;  echo "$id,$a"?>)" >
	
</footer>
</html>
