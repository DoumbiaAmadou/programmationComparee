from data import *

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
