 # -*- coding: utf-8 -*-

import json
import urllib
import urllib2
import pprint
from cookielib import CookieJar
from antcommand import *

base_url = "https://yann.regis-gianas.org/antroid/0"
cj = CookieJar()
opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cj))
pp = pprint.PrettyPrinter(indent=4)

def makeURL(command):
	return "%s/%s" %(base_url, command)

'''
Affiche les methodes d'API sur la sortie standart.
'''
def showAPI():
	response = opener.open(makeURL("api"))
	print response.read()

'''
Affiche le pseudonyme d'utilisateur sur la sortie standart.
'''
def whoami():
	response = opener.open(makeURL("whoami"))
	print response.read()

'''
Inscription en utilisant un nom "login" et un mot de passe "password".

Retourne True si l'inscription a ete effectuee.
En case d'erreur affiche l'erreur sur la sortie standart et retourne False.
'''
def register_user(login, password):
	query_args = { 'login': login, 'password': password }
	data = urllib.urlencode(query_args)

	response = opener.open(makeURL("register"), data)

	data = json.loads(response.read())
	if data["status"] == "completed":
		print "Successfully registered as, %s" %(login)
		return True
	else:
		print data["response"]["error_msg"]
		return False

'''
Se connecter en utilisant un nom "login" et un mot de passe "password".

Retourne True si la connection a ete effectuee. 
En case d'erreur affiche l'erreur sur la sortie standart et retourne False.
'''
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


'''
Se deconnecter.

Retourne True si la deconnection a ete effectuee.
En case d'erreur affiche l'erreur sur la sortie standart et retourne False.
'''
def logout():
	response = opener.open(makeURL("logout"))
	data = json.loads(response.read())

	if data["status"] == "completed":
		print "Successfully logged out"
		return True
	else:
		print data["response"]["error_msg"]
		return False


'''
Recuperer la liste de jeux courantes.

Retourne la liste de dictionnaires avec de donnees sur les jeux. 
En cas d'erreur affiche l'erreur sur la sortie standart et retourne None.
'''
def get_games():
	response = opener.open(makeURL("games"))

	data = json.loads(response.read())
	if data["status"] == "completed":
		return data["response"]["games"]
	else:
		print data["response"]["error_msg"]

'''
Detruir un jeu avec l'identifcateur "game_id"
Retourne True en cas de destruction du jeu reussi.
En case d'erreur affiche l'erreur sur la sortie standart et retourne False.
'''
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

'''
Creer un jeu
Retourne l'identifcateur du jeu en cas de creation reussi.
En case d'erreur affiche l'erreur sur la sortie standart et retourne None.
'''
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

'''
Affiche le log du jeu sur la sortie standart.
Attention: 	
La requete ne retourne pas erreur en cas d'erreur (on ne voit que Internal Error 500)
	Pour que cette methode marche faut
	- etre connecte (faire login)
	- que le jeu soit fini ("status": "over")
'''
def show_game_log(game_id):
	query_args = { 'id':game_id }
	data = urllib.urlencode(query_args)

	response = opener.open(makeURL("log")+"?"+data)
	print response.read()

'''
Recuperer un statut de jeu avec l'identifcateur "game_id".

Retourne un dictionnaire avec de donnees sur le statut du jeu. 
En cas d'erreur affiche l'erreur sur la sortie standart et retourne None.
'''
def game_status(game_id):
	query_args = { 'id':game_id }
	data = urllib.urlencode(query_args)

	response = opener.open(makeURL("status")+"?"+data)

	data = json.loads(response.read())
	if data["status"] == "completed":
		return data["response"]["status"]
	else:
		print data["response"]["error_msg"]

'''
Connection au jeu avec l'identifcateur "game_id".

Retourne True en cas de connection reussie. 
En cas d'erreur affiche l'erreur sur la sortie standart et retourne False.
'''
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


'''
Faire un tour du jeu avec l'identifcateur "game_id".
"attached_commands" - une liste d'objets AttachedCommand (voir antcommand.py)

Retourne une liste de dictionnaires d'observations des fourmis en cas du tour reussi. 
En cas d'erreur affiche l'erreur sur la sortie standart et retourne None.
'''

def game_play(game_id, attached_commands):
	command_list = ""

	for command in attached_commands[:-1]:
		command_list += command.rawValue()+","
	last_command = attached_commands[-1]
	command_list += last_command.rawValue()

	query_args = { 'id':game_id, 'cmds': command_list }
	data = urllib.urlencode(query_args)

	response = opener.open(makeURL("play")+"?"+data)
	data = json.loads(response.read())
	if data["status"] == "completed":
		return data["response"]["observations"]
	else:
		print data["response"]["error_msg"]

# Tests
def test():
	if login("vlad", "muravei"):
		gid = game_create(teaser='Test',users='vlad',pace=50, nb_turn=100, nb_ant_per_player=3, nb_player=2, minimal_nb_player=1, initial_energy=100, initial_acid=50)
		print game_status(gid)
		print get_games()
		game_join(gid)
		pp.pprint(game_play(gid, [AttachedCommand(0, Left()), AttachedCommand(1, Forward())]))
		print game_status(gid)
		game_destroy(gid)
		logout()

test()