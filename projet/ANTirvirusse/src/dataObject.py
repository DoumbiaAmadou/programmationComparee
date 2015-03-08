from network_layer import *
from antcommand import *

class Ant():

	def __init__(self, ident, energy, acid, brain, position):
		self.ident = ident
		self.energy = energy
		self.acid = acid
		self.brain = brain
		self.position = position

	def get_position (self):
		return position

	def set_position (self, position):
		self.position = position

	def add_hp (self, hp):
		self.energy += hp

	def minus_hp (self, hp):
		self.energy -= hp

	def add_acid (self, acid):
		self.acid += acid

	def minus_acid (self, acid):
		self.acid -= acid

class Case():

	def __init__(self, type, x, y):
		self.type = type
		self.x = x
		self.y = y

	def is_not_moved (self):
		if is_food (x,y):
			return TypeOfCase.Food
		if is_obstacle (x,y):
			return TypeOfCase.Obstacle

	def is_food (self):
		if Map.get_type_of_case_at (x,y) == Food.Wheat or Map.get_type_of_case_at (x,y) == Food.Fromage or Map.get_type_of_case_at (x,y) == Food.Sugar: 
		   return True

	def is_obstacle (self):
		if Map.get_type_of_case_at (x,y) == Obstacle.Rock or Map.get_type_of_case_at (x,y) == Obstacle.Water or Map.get_type_of_case_at (x,y) == Obstacle.Obstacle : 
		   return True


class TypeOfCase():
	Empty = 0
	Food = 1
	Obstacle = 2

class Obstacle():
	Rock = 0
	Water = 1
	Grass = 2

class Food():
	Wheat = 0
	Fromage = 1
	Sugar = 2

class Map():

	def __init__(self, line, col, nb_ant, nb_enemy):
		self.line = line
		self.col = col
		self.nb_ant = nb_ant
		self.nb_enemy = nb_enemy
		self.matrix = [[0 for x in range(line)] for x in range(col)]

	def scan_map(self):
		for l in line:
			for c in col:
				matrix[l][c] = TypeOfCase.Empty # just false values
		return matrix

	def get_type_of_case_at(self, x, y):
		# game_log, return empty or not
		return maxtrix[x][y]

	def enemy_position(self):
		# game_log, all positions of enemy
		pass

	def ally_position(self):
		# game_log, all positions of ally
		pass

class Action():
	TurnLeft = 1
	TurnRight = 2
	Rest = 3
	Attack = 4
	Forward = 5

