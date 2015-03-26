 # -*- coding: utf-8 -*-

class Case():

	def __init__(self, type, x, y):
		self.type = type
		self.x = x
		self.y = y

	def __str__(self):
		return "Case type: %s [x: %i, y: %i]" %(self.type, self.x, self.y)

	def is_food (self):
		return self.type == Food.Wheat or self.type == Food.Cheese or self.type == Food.Sugar 

	def is_rock (self):
		return self.type == Obstacle.Rock

	def is_water (self):
		return self.type == Obstacle.Water

	def is_grass (self):
		return self.type == Obstacle.Grass


class TypeOfCase():
	Empty 	 = 'empty'
	Food 	 = 'food'
	Obstacle = 'obstacle'

class Obstacle():
	Rock 	= 'rock'
	Water 	= 'water'
	Grass 	= 'grass'

class Food():
	Wheat 	= 'wheat'
	Cheese 	= 'cheese'
	Sugar 	= 'sugar'