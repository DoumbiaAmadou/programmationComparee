 # -*- coding: utf-8 -*-
from case import Case
from ant import Ant

class GameMap():

	def __init__(self, nb_ants, initial_energy, initial_acid):
		self.map_dict = {}
		self.ants     = []

		for ant_id in range(0, nb_ants):
		    ant = Ant(ant_id, initial_energy, initial_acid, True)
		    self.ants.append(ant)

	def __str__(self):
		text_repr = ""
		
		for coords, case in self.map_dict.iteritems():
			text_repr += "%s \t: %s\n" %(coords, case)

		return text_repr

	def get_ants(self):
	    return self.ants

	def get_ant_with_id(self, ant_id):
	    search_result = filter(lambda ant: ant.ant_id == ant_id, self.ants)
	    if len(search_result) > 0:
	        return search_result[0]
	    else:
	        return None

	def update(self, ants_stats, verbose=False):
		for stat in ants_stats:
			cases = map(lambda case_dict: Case(case_dict["content"]["kind"], int(case_dict["x"]), int(case_dict["y"])), stat[1])
			for case in cases:
				self.map_dict[(case.x, case.y)] = case

			ant_state   = stat[0]
			ant_id      = ant_state["id"]
			current_ant = self.get_ant_with_id(ant_id)

			if current_ant is not None:
				current_ant.set_energy(int(ant_state["energy"]))
				current_ant.set_acid(int(ant_state["acid"]))
				current_ant.set_brain(ant_state["brain"]=="controlled")
				current_ant.set_position(int(ant_state["x"]), int(ant_state["y"]))
				if verbose:
					print(current_ant)

		if verbose:
			print(self)

	def get_case_at(self, x, y):
		return map_dict[(x, y)]