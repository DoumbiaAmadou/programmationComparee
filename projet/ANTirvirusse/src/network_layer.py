import json
import urllib
import urllib2
from cookielib import CookieJar

base_url = "https://yann.regis-gianas.org/antroid/0"
cj = CookieJar()
opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cj))

def makeURL(command):
	return "%s/%s" %(base_url, command)


def showAPI():
	response = opener.open(makeURL("api"))

	print response.read()

def whoami():
	response = opener.open(makeURL("whoami"))

	print response.read()

def register_user(login, password):
	query_args = { 'login': login, 'password': password }
	data = urllib.urlencode(query_args)

	response = opener.open(makeURL("register"), data)
	response_text = response.read()

	print response_text

def login(login, password):
	query_args = { 'login': login, 'password': password }
	data = urllib.urlencode(query_args)

	response = opener.open(makeURL("auth"), data)
	data = json.loads(response.read())

	if data["status"] == "completed":
		print "Successfully logged in, %s" %(login)
		return True
	else:
		print data["response"]["error_msg"]
		return False



def logout():
	response = opener.open(makeURL("logout"))
	data = json.loads(response.read())

	if data["status"] == "completed":
		print "Successfully logged out"
		return True
	else:
		print data["response"]["error_msg"]
		return False


def get_games():
	response = opener.open(makeURL("games"))

	data = json.loads(response.read())
	if data["status"] == "completed":
		return data["response"]["games"]
	else:
		print data["response"]["error_msg"]

def game_destroy(game_id):
	query_args = { 'id':game_id }
	data = urllib.urlencode(query_args)

	response = opener.open(makeURL("destroy")+"?"+data)
	data = json.loads(response.read())
	if data["status"] == "completed":
		print "Game %s was successfully destroyed" %(game_id)
		return True
	else:
		print data["response"]["error_msg"]
		return False

def game_create(pace, nb_turn, nb_ant_per_player, nb_player, minimal_nb_player, initial_energy, initial_acid, users='+', teaser=""):
	if pace < 1 or pace > 100:
		print "Pace out of range. Possible: [1..100]"

	if nb_turn < 1 or nb_turn > 100000:
		print "Number of turn out of range. Possible: [1..100000]"

	if nb_ant_per_player < 1 or nb_ant_per_player > 42:
		print "Number of turn out of range. Possible: [1..42]"

	if nb_player < 1 or nb_player > 42:
		print "Number of turn out of range. Possible: [1..42]"

	if minimal_nb_player < 1 or minimal_nb_player > nb_player:
		print "Minimal number of players out of range. Possible: [1..%i]" %(nb_player)

	if initial_energy < 1 or initial_energy > 1000:
		print "Initial energy out of range. Possible: [1..1000]"

	if initial_acid < 1 or initial_acid > 1000:
		print "Initial acid out of range. Possible: [1..1000]" 

	query_args = { 	'users'				: users, 
					'teaser'			: teaser,
					'pace'				: pace,
					'nb_turn'			: nb_turn,
					'nb_ant_per_player'	: nb_ant_per_player,
					'nb_player'			: nb_player,
					'minimal_nb_player'	: minimal_nb_player,
					'initial_energy'	: initial_energy,
					'initial_acid' 		: initial_acid
					}

	data = urllib.urlencode(query_args)	

	response = opener.open(makeURL("create")+"?"+data)

	data = json.loads(response.read())
	if data["status"] == "completed":
		game_id = data["response"]["identifier"]
		print "Game created: %s" %(game_id)
		return game_id
	else:
		print data["response"]["error_msg"]


def game_log(game_id):
	#not working
	query_args = { 'id':game_id }
	data = urllib.urlencode(query_args)

	response = opener.open(makeURL("log")+"?"+data)
	response_text = response.read()

	print response_text

def game_status(game_id):
	query_args = { 'id':game_id }
	data = urllib.urlencode(query_args)

	response = opener.open(makeURL("status")+"?"+data)

	data = json.loads(response.read())
	if data["status"] == "completed":
		return data["response"]["status"]
	else:
		print data["response"]["error_msg"]

def game_join(game_id):
	query_args = { 'id':game_id }
	data = urllib.urlencode(query_args)

	response = opener.open(makeURL("join")+"?"+data)
	data = json.loads(response.read())
	if data["status"] == "completed":
		print "Successfully joined game %s" %(game_id)
		return True
	else:
		print data["response"]["error_msg"]
		return False

def game_play(game_id, commands):
	query_args = { 'id':game_id, 'cmds': commands }
	data = urllib.urlencode(query_args)

	response = opener.open(makeURL("play")+"?"+data)
	response_text = response.read()

	print response_text

def test():
	if login("vlad", "muravei"):
		gid = game_create(teaser='Test',users='vlad',pace=50, nb_turn=50000, nb_ant_per_player=3, nb_player=2, minimal_nb_player=1, initial_energy=100, initial_acid=50)
		print game_status(gid)
		print get_games()
		game_join(gid)
		game_play(gid, "0:left,1:forward")
		print game_status(gid)
		game_destroy(gid)
		logout()

showAPI()
# test()