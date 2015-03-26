<?php
include('BIblio.php') ; 
/*
*
* Preview version    in Javascript.
*
*
*
*
*
*/
isset($_GET["wait"] && $_GET["wait"]==1 && $_GET["id"] /*&& $_GET["cmd"]*/ ){
?>

<html>
<head>
<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
<script>
function callback(id,wait){
	var id = document.getElementById(id) ;
	var cmd = "reset"//document.getElementById(id) ;
	var cmd = document.getElementById(id) ;
	 
	setTimeout(function(){ 
			location.replace("localhost/badin/Pcomp/waitGame.php?id="+$_GET["id"]+"&wait="+ )}
				,wait); 	
	 location.replace("localhost/badin/Pcomp/waitGame.php?id="+$_GET["id"]+"&wait="); 
}


</script>
</head>
<body>

<a href=""></a>

<?php 

 	$st = status($_GET["id"]) ; 
	$resultat =play($_GET["id"], $_GET["cmd"]); 
	
}
$id = $_GET["id"] ;
$st = status($id);

echo "<div id =\"id\">$id</div>";
echo "<div id =\"sleepTime\">$sleeptime = 60/$st->response->status->pace</div>";
echo "<a id=\"ed\" href=""></a>"; 

//print_r(gettype($st->response->status->pace))  ; 

print_r($st->response->status->turn) ;

//$ste = dolog("854927805303702988149600388713876543") ; 
//print_r($st); 
//
//observe ($id); 

$sleeptime = 60/$st->response->status->pace ; 
echo"<br> sleep time = ".$sleeptime ; 
echo "<br/>";

}
?>
</body>
<footer onload="callback(<?php echo "$_GET["id"]" ?>)">
	
</footer>
</html>