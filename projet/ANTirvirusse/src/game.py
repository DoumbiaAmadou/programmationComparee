 # -*- coding: utf-8 -*-

from dataObject import *
import network_layer as nl
import pprint

class Player():

	def __init__(self, ident, nb_turn, nb_ant):
		self.ident = ident
		self.nb_turn = nb_turn
		self.nb_ant = nb_ant

	def set_up_game (self):
		pass

	def move (self, idAnt):
		pass

	def attack_fourmi (self, idAntEnemy):
		pass

	def hack_fourmi (self, idAntEnemy):
		pass

def create_game(pace, nb_turn, nb_ant_per_player, nb_player, minimal_nb_player, initial_energy, initial_acid, users='+', teaser=""):
	game_id = nl.game_create(pace, nb_turn, nb_ant_per_player, nb_player, minimal_nb_player, initial_energy, initial_acid, users, teaser)
	if game_id is not None:
		status = nl.game_status(game_id)
		game_state = status["status"]["status"]
		initial_energy = status["initial_energy"]
		nb_ant_per_player = status["nb_ant_per_player"]
		initial_acid = status["initial_acid"]
		pace = status["pace"]
		game = Game(game_id, nb_ant_per_player, initial_acid, initial_energy, pace)
		return game
	else:
		return None

def join_game(game_id):
	status = nl.game_status(game_id)
	game_state = status["status"]["status"]
	if game_state != "playing":
		print "Impossible to join game. Current status: %s" %(game_state)
		return None
	else:
		nb_ant_per_player = status["nb_ant_per_player"]
		initial_acid = status["initial_acid"]
		initial_energy = status["initial_energy"]
		pace = status["pace"]
		return Game(game_id, nb_ant_per_player, initial_acid, initial_energy, pace)


class Game():

	def __init__(self, gid, nb_ants, initial_acid, initial_energy, pace):
		nl.game_join(gid)
		self.gid = gid
		self.pace = pace
		self.ants = []
		self.game_map = {}

		for ant_id in range(0, nb_ants):
			ant = Ant(ant_id, initial_energy, initial_acid, True)
			self.ants.append(ant)

		self.make_move()

	def destroy(self):
		nl.game_destroy(self.gid)

	def make_move(self):
		commands = map(lambda ant: AttachedCommand(ant.ident, ant.next_command), self.ants)
		ants_stats = game_play(self.gid, commands)

		for stat in ants_stats:
			ant_state = stat[0]
			ant_id = ant_state["id"]
			current_ant = filter(lambda ant: ant.ident == ant_id, self.ants)[0]

			if current_ant is not None:
				current_ant.set_energy(int(ant_state["energy"]))
				current_ant.set_acid(int(ant_state["acid"]))
				current_ant.set_brain(ant_state["brain"]=="controlled")
				current_ant.set_position(int(ant_state["x"]), int(ant_state["y"]))

			cases = map(lambda case_dict: Case(case_dict["content"]["kind"], int(case_dict["x"]), int(case_dict["y"])), stat[1])
			for case in cases:
				self.game_map[(case.x, case.y)] = case

		for ant in self.ants:
			print ant

		for key, value in self.game_map.iteritems():
			print "%s \t: %s" %(key, value)

	def show_state():
		print nl.game_status(self.game_id)


if nl.login("vlad", "muravei"):
	game = create_game(teaser='Test',users='vlad',pace=50, nb_turn=100, nb_ant_per_player=3, nb_player=2, minimal_nb_player=1, initial_energy=100, initial_acid=50)
	game.destroy()