 # -*- coding: utf-8 -*-
from case import Case

class GameMap():

	def __init__(self):
		self.map_dict = {}

	def __str__(self):
		text_repr = ""
		
		for coords, case in self.map_dict.iteritems():
			text_repr += "%s \t: %s\n" %(coords, case)

		return text_repr

	def update(self, ants_stats):
		for stat in ants_stats:
			cases = map(lambda case_dict: Case(case_dict["content"]["kind"], int(case_dict["x"]), int(case_dict["y"])), stat[1])
			for case in cases:
				self.map_dict[(case.x, case.y)] = case

	def get_case_at(self, x, y):
		return map_dict[(x, y)]