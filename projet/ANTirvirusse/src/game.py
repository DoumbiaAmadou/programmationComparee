 # -*- coding: utf-8 -*-

from Donnees.game_map import GameMap
from Donnees.ant import Ant
from antcommand import AttachedCommand
from Network import network_layer as nl

class Game():

    @classmethod
    def state_of_game(cls, game_id):
        print nl.game_status(game_id)

    def __init__(self, gid, nb_ants, initial_acid, initial_energy, pace, save):
        nl.game_join(gid)
        self.gid            = gid
        self.serializer     = GameSerializer(gid) if save else None
        self.pace           = pace
        self.initial_acid   = initial_acid
        self.initial_energy = initial_energy
        self.game_map       = GameMap(nb_ants, initial_energy, initial_acid)
        self.make_move()

    def get_initial_acid(self):
        return self.initial_acid

    def get_initial_energy(self):
        return self.initial_energy

    def get_map(self):
        return self.game_map

    def destroy(self):
        nl.game_destroy(self.gid)

    def replay_game(self, save_file):
        nl.game_replay(self.gid, save_file)

    def make_move(self):
        commands = map(lambda ant: ant.get_attached_command(), self.game_map.get_ants())
        ants_stats = nl.game_play(self.gid, commands, self.serializer)
        self.game_map.update(ants_stats, verbose=True)

    def show_status(self):
        print nl.game_status(self.gid)