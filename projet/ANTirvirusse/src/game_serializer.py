#!/usr/bin/env python
# -*- coding: UTF-8 -*-
import os, subprocess
from array import *
import antcommand


class GameSerializer(object):
	def __init__(self, game_id):
		self.file_path = "game%i.save" %(game_id)
		self.turns = []

	def add_turn(self, commands):
		self.turns.append(commands)

	def generate_save(self):
		dir = os.path.dirname("./"+self.file_path)
		try:
			os.stat(dir)
		except:
			os.makedirs(dir)

		save_file = open("./"+self.file_path, "w")

		for turn in self.turns:
			save_file.write(turn+'\n')
		save_file.close()


def test():
	gs = GameSerializer(5)
	gs.add_turn("turn1")
	gs.add_turn("turn2")
	gs.add_turn("turn3")
	gs.generate_save()

	GameSerializer.read_save("game5.save")

