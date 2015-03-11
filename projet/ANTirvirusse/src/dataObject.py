from network_layer import *
from antcommand import *

class Ant():

	def __init__(self, ident, energy, acid, brain, x, y):
		self.ident = ident
		self.energy = energy
		self.acid = acid
		self.brain = brain
		self.x = x
		self.y = y
		self.nextCommand = Rest()

	def get_id (self):
		return ident

	def add_hp (self, hp):
		self.energy += hp

	def minus_hp (self, hp):
		self.energy -= hp

	def get_energy (self):
		return energy

	def set_energy (self, energy):
		this.energy = energy

	def add_acid (self, acid):
		self.acid += acid

	def minus_acid (self, acid):
		self.acid -= acid

	def get_acid (self):
		return acid

	def set_acid (self, acid):
		this.acid = acid

	def get_brain (self):
		return brain

	def set_brain (self, brain):
		this.brain = brain

	def get_position (self):
		return x, y

	def set_position (self, x, y):
		self.x = x
		self.y = y

	def rest():
		nextCommand = Rest()

	def forward():
		nextCommand = Forward()

	def left():
		nextCommand = Left()

	def right():
		nextCommand = Right()

	def attack(level):
		nextCommand = Attack(level)

	def hack(instructions):
		nextCommand = Hack(instructions)

class Case():

	def __init__(self, type, x, y):
		self.type = type
		self.x = x
		self.y = y

	def is_food (self):
		if Map.get_type_of_case_at (x,y) == Food.Wheat or Map.get_type_of_case_at (x,y) == Food.Fromage or Map.get_type_of_case_at (x,y) == Food.Sugar: 
		   return True

	def is_obstacle (self):
		if Map.get_type_of_case_at (x,y) == Obstacle.Rock or Map.get_type_of_case_at (x,y) == Obstacle.Water or Map.get_type_of_case_at (x,y) == Obstacle.Obstacle: 
		   return True

class TypeOfCase():
	Empty = 'empty'
	Food = 'food'
	Obstacle = 'Obstacle'

class Obstacle():
	Rock = 'rock'
	Water = 'water'
	Grass = 'grass'

class Food():
	Wheat = 'wheat'
	Cheese = 'cheese'
	Sugar = 'sugar'

class Map():

	def __init__(self, line, col, nb_ant):
		self.line = line
		self.col = col
		self.nb_ant = nb_ant
		self.matrix = [[0 for x in range(line)] for x in range(col)]

	def scan_map(self):
		for l in line:
			for c in col:
				matrix[l][c] = TypeOfCase.Empty # just false values
		return matrix

	def get_info (self, x, y, gid, cmds):
		game = game_play (gid, cmds)
		ants = []
		for i in range(nb_ant):

			ident = game[i][0]['id']
			energy = game[i][0]['energy']
			acid = game[i][0]['acid']
			brain = game[i][0]['brain']
			x = game[i][0]['x']
			y = game[i][0]['y']

			ant1 = Ant(ident, energy, acid, brain, x, y)
			ants.append(ant1)

			matrix[x-1][y-1] = game[i][1][0]['content']['kind']
			matrix[x][y-1] = game[i][1][1]['content']['kind']
			matrix[x+1][y-1] = game[i][1][2]['content']['kind']
			matrix[x-1][y] = game[i][1][3]['content']['kind']
			matrix[x][y] = game[i][1][4]['content']['kind']
			matrix[x+1][y] = game[i][1][5]['content']['kind']
			matrix[x-1][y+1] = game[i][1][6]['content']['kind']
			matrix[x][y+1] = game[i][1][7]['content']['kind']
			matrix[x+1][y+1] = game[i][1][8]['content']['kind']

	def get_type_of_case_at(self, x, y):
		# game_log, return empty or not
		return maxtrix[x][y]
''' # not use
	def enemy_position(self):
		# all positions of enemy
		pass

	def ally_position(self):
		# all positions of ally
		pass
'''
class Action():
	TurnLeft = 1
	TurnRight = 2
	Rest = 3
	Attack = 4
	Forward = 5

