 # -*- coding: utf-8 -*-

from game_map import GameMap
from ant import Ant
from antcommand import AttachedCommand
import network_layer as nl

class Game():

    @classmethod
    def state_of_game(cls, game_id):
        print nl.game_status(game_id)

    def __init__(self, gid, nb_ants, initial_acid, initial_energy, pace):
        nl.game_join(gid)

        self.gid            = gid
        self.pace           = pace
        self.ants           = []
        self.game_map       = GameMap()
        self.initial_acid   = initial_acid
        self.initial_energy = initial_energy

        for ant_id in range(0, nb_ants):
            ant = Ant(ant_id, initial_energy, initial_acid, True)
            self.ants.append(ant)

        self.make_move()

    def get_initial_acid(self):
        return self.initial_acid

    def get_initial_energy(self):
        return self.initial_energy

    def get_map(self):
        return self.game_map

    def get_ants(self):
        return self.ants

    def get_ant_with_id(self, ant_id):
        search_result = filter(lambda ant: ant.ant_id == ant_id, self.ants)
        if len(search_result) > 0:
            return search_result[0]
        else:
            return None

    def destroy(self):
        nl.game_destroy(self.gid)

    def make_move(self):
        commands = map(lambda ant: ant.get_attached_command(), self.ants)
        ants_stats = nl.game_play(self.gid, commands)
        self.game_map.update(ants_stats)

        for stat in ants_stats:
            ant_state   = stat[0]
            ant_id      = ant_state["id"]
            current_ant = self.get_ant_with_id(ant_id)

            if current_ant is not None:
                current_ant.set_energy(int(ant_state["energy"]))
                current_ant.set_acid(int(ant_state["acid"]))
                current_ant.set_brain(ant_state["brain"]=="controlled")
                current_ant.set_position(int(ant_state["x"]), int(ant_state["y"]))

                print(current_ant)

        print(self.game_map)

    def show_status(self):
        print nl.game_status(self.gid)