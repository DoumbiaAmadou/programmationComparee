 # -*- coding: utf-8 -*-
 
from antcommand import *

class Ant():

	def __init__(self, ant_id, energy, acid, brain, x=0, y=0):
		self.ant_id = ant_id
		self.energy = energy
		self.acid 	= acid
		self.brain 	= brain
		self.x = x
		self.y = y
		self.dx = 0
		self.dy = 0
		self.next_command = Left()

	def update_state(self, ant_state):
		self.energy = int(ant_state["energy"])
		self.acid 	= int(ant_state["acid"])
		self.brain 	= ant_state["brain"]=="controlled"
		self.set_position(int(ant_state["x"]), int(ant_state["y"]))
		self.set_orientation(int(ant_state["dx"]), int(ant_state["dy"]))

	def __str__(self):
		return "Ant id: %i brain: %r energy: %i acid: %i [x: %i, y: %i] [dx: %i, dy: %i]" %(self.ant_id, self.brain, self.energy, self.acid, self.x, self.y, self.dx, self.dy)

	def get_id (self):
		return ant_id

	def get_energy (self):
		return energy

	def set_energy (self, energy):
		self.energy = energy

	def add_acid (self, acid):
		self.acid += acid

	def minus_acid (self, acid):
		self.acid -= acid

	def get_acid (self):
		return acid

	def set_acid (self, acid):
		self.acid = acid

	def get_brain (self):
		return brain

	def set_brain (self, brain):
		self.brain = brain

	def get_position (self):
		return x, y

	def set_position (self, x, y):
		self.x = x
		self.y = y

	def get_orientation(self):
		return self.dx, self.dy

	def set_orientation(self, dx, dy):
		self.dx = dx
		self.dy = dy

	def get_attached_command(self):
		return AttachedCommand(self.ant_id, self.next_command)

	def rest():
		self.next_command = Rest()

	def forward():
		self.next_command = Forward()

	def left():
		self.next_command = Left()

	def right():
		self.next_command = Right()

	def attack(level):
		self.next_command = Attack(level)

	def hack(instructions):
		self.next_command = Hack(instructions)

