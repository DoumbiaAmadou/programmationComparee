<?php


$url  = "http://yann.regis-gianas.org/antroid/0/" ; 

function decode ($var){
	return json_decode($var) ; 
}

function auth($login , $password){
	$curl = curl_init() ; 
	$body = array( 'login' => $login , 'password' => $password); 
	
	$url  = "http://yann.regis-gianas.org/antroid/0/" ; 
	curl_setopt($curl, CURLOPT_URL, $url."auth");
	curl_setopt ($curl, CURLOPT_POST, true);
	curl_setopt ($curl, CURLOPT_POSTFIELDS, $body);

	

	$tmpfname = dirname(__FILE__).'/cookie.txt';
    curl_setopt($curl, CURLOPT_COOKIEJAR, $tmpfname);
    curl_setopt($curl, CURLOPT_COOKIEFILE, $tmpfname);
    curl_setopt($curl, CURLOPT_HEADER, false);
  
    curl_setopt($curl, CURLOPT_FOLLOWLOCATION, true);

	$page = curl_exec ($curl);
	curl_close ($curl);

	$var = json_decode($page , true);
}
function register($login , $password){
	$curl = curl_init() ; 
	curl_setopt($curl, CURLOPT_URL, $url."register");
	curl_setopt ($curl, CURLOPT_POST, false);
	curl_setopt ($curl, CURLOPT_POSTFIELDS, $body);
	curl_setopt($curl,CURLOPT_POSTFIELDS,
		array(
			"login" => $login,
			"possword" =>$password
		 )); 
	$page = curl_exec ($curl);
	curl_close ($curl);
	
	echo "debug register <br>";
	return decode(page); 
}

function destroy(){

	$curl = curl_init() ; 
	curl_setopt($curl, CURLOPT_URL, $url."destroy");
	//curl_setopt ($curl, CURLOPT_POST, false);
	//curl_setopt ($c, CURLOPT_POSTFIELDS, $body);
	$page = curl_exec ($curl);
	curl_close ($curl);
	curl_setopt($curl,CURLOPT_POSTFIELDS,
		array(
			"id" => "DDTI")
		) ; 
	echo "no probléme <br>";
	return decode(page); 
}
function games(){
		$url  = "http://yann.regis-gianas.org/antroid/0/";
		$curl = curl_init();
		curl_setopt($curl, CURLOPT_URL, $url.'games');
		curl_setopt ($curl, CURLOPT_RETURNTRANSFER, true);
		$page = curl_exec ($curl);
		curl_close ($curl);
		return json_decode($page) ; 
	}
function joinGame($id){
	$curl = curl_init() ; 
	$body = array( 'id' => $id ); 
	
	$url  = "http://yann.regis-gianas.org/antroid/0/join?".http_build_query($body) ; 
	echo $url ; 
	curl_setopt($curl, CURLOPT_URL, $url);
	curl_setopt ($curl, CURLOPT_POST, false);
	$tmpfname = dirname(__FILE__).'/cookie.txt';
    curl_setopt($curl, CURLOPT_COOKIEFILE, $tmpfname);
	curl_setopt ($curl, CURLOPT_RETURNTRANSFER, true);
	$page = curl_exec ($curl);
	curl_close ($curl);
	$var = json_decode($page);
	print_r($var);
	return $var ;
}
function create($id , 
		$teaser ,
		$pace , 
		$nbturn , 
		$nbAntPlayer , 
		$nbplayer , 
		$minNbPlayer , 
		$initEnergy , 
		$initAcid
		){
	$curl = curl_init() ; 
	$body = array( 'users' => $id , 
					'teaser' =>   $teaser, 
					'pace' => $pace,
					'nb_turn' => $nbturn , 
					'nb_ant_per_player' => $nbAntPlayer , 
					'nb_player' => $nbplayer ,
					'minimal_nb_player' => $minNbPlayer ,
					'initial_energy' => $initEnergy , 
					'initial_acid'=> $initAcid
				 ); 
	
	$url  = "http://yann.regis-gianas.org/antroid/0/create?".http_build_query($body) ; 
	echo $url ; 
	curl_setopt($curl, CURLOPT_URL, $url);
	curl_setopt ($curl, CURLOPT_POST, false);
	$tmpfname = dirname(__FILE__).'/cookie.txt';
    curl_setopt($curl, CURLOPT_COOKIEFILE, $tmpfname);
	curl_setopt ($curl, CURLOPT_RETURNTRANSFER, true);
	$page = curl_exec ($curl);
	curl_close ($curl);
	$var = json_decode($page);
	echo"<br> create : " ;
	print_r($var);
	echo"<br>";
	return $var ;
}

function dolog($id){
$curl = curl_init() ; 
	$body = array( 'id' => $id ); 
	$url  = "http://yann.regis-gianas.org/antroid/0/log?".http_build_query($body) ; 
	echo $url ; 
	curl_setopt($curl, CURLOPT_URL, $url);
	curl_setopt ($curl, CURLOPT_POST, false);
	$tmpfname = dirname(__FILE__).'/cookie.txt';
    curl_setopt($curl, CURLOPT_COOKIEFILE, $tmpfname);
	curl_setopt ($curl, CURLOPT_RETURNTRANSFER, true);
	$page = curl_exec ($curl);
	curl_close ($curl);
	$var = json_decode($page);
	print_r($var);
	return $var ;
}

function observe ($id , $antid) {
	$url = array('ant'=> $id , 
				'antid'=> $antid) ; 
	$curl = curl_init();

	curl_setopt($curl, CURLOPT_URL, $url.http_build_query($url));
	curl_setopt ($curl, CURLOPT_POST, false);
	//curl_setopt ($c, CURLOPT_POSTFIELDS, $body);
	$tmpfname = dirname(__FILE__).'/cookie.txt';
    curl_setopt($curl, CURLOPT_COOKIEFILE, $tmpfname);
	curl_setopt ($curl, CURLOPT_RETURNTRANSFER, true);
	$page = curl_exec ($curl);
	curl_close ($curl);
	return decode($page); 
}

function play($id ,$cmd){
	$body= array('id'=> $id , 				'cmds'=> $cmd ) ; 
	$url  = "http://yann.regis-gianas.org/antroid/0/play?"; 	
	$curl = curl_init();
	curl_setopt($curl, CURLOPT_URL, $url.http_build_query($body));
	curl_setopt ($curl, CURLOPT_POST, false);
	//curl_setopt ($c, CURLOPT_POSTFIELDS, $body);
	$tmpfname = dirname(__FILE__).'/cookie.txt';
    curl_setopt($curl, CURLOPT_COOKIEFILE, $tmpfname);
	curl_setopt ($curl, CURLOPT_RETURNTRANSFER, true);
	$page = curl_exec ($curl);
	curl_close ($curl);
	
	return decode($page); 
}

function shutdown(){

	$curl = curl_init();
	curl_setopt($curl, CURLOPT_URL, $url."shutdown");
	//curl_setopt ($curl, CURLOPT_POST, false);
	//curl_setopt ($c, CURLOPT_POSTFIELDS, $body);
	curl_setopt ($curl, CURLOPT_RETURNTRANSFER, true);
	$page = curl_exec ($curl);
	curl_close ($curl);
	curl_setopt(
		$curl,
		CURLOPT_POSTFIELDS,
		array("id" => "DDTI")
		);
	

	echo "no probléme <br>";
	return decode(page); 
}
function status($id){
	
	$body = array('id'=> $id);
	$url  = "http://yann.regis-gianas.org/antroid/0/status?".http_build_query($body) ; 
	
	$curl = curl_init();
	curl_setopt($curl, CURLOPT_URL, $url);
	curl_setopt ($curl, CURLOPT_POST, false);
	$tmpfname = dirname(__FILE__).'/cookie.txt';
    curl_setopt($curl, CURLOPT_COOKIEFILE, $tmpfname);
	curl_setopt ($curl, CURLOPT_RETURNTRANSFER, true);
	$page = curl_exec ($curl);
	curl_close ($curl);
	echo "Status response <br>";
	return decode($page); 
}

function playGame(){
	
}

function whoami(){
	$curl = curl_init();
	curl_setopt($curl, CURLOPT_URL, $url."shutdown");
	//curl_setopt ($curl, CURLOPT_POST, false);
	//curl_setopt ($c, CURLOPT_POSTFIELDS, $body);
	curl_setopt ($curl, CURLOPT_RETURNTRANSFER, true);
	$page = curl_exec ($curl);
	curl_close ($curl);
	echo "no probléme <br>";
	return decode(page); 
}

function logout(){

	$curl = curl_init();
	curl_setopt($curl, CURLOPT_URL, $url."logout");
	//curl_setopt ($curl, CURLOPT_POST, false);
	//curl_setopt ($c, CURLOPT_POSTFIELDS, $body);
	curl_setopt ($curl, CURLOPT_RETURNTRANSFER, true);
	$page = curl_exec ($curl);
	curl_close ($curl);

	echo "no probléme <br>";
	return decode(page); 
}

?>




