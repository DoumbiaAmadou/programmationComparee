# -*- coding: utf-8 -*-

from .. import game as g
from . import strategy_decision_maker


class IAPlayer():
    def __init__(self, login, pwd):
        self.strategy = None
        self.decisionMaker = strategy_decision_maker.StrategyDecisionMaker()
        self.game = None
        self.login = login
        self.pwd = pwd

    def create_game(self, pace, nb_turn, nb_ant_per_player, nb_player, minimal_nb_player, initial_energy, initial_acid):
        self.game = g.create_game(pace, nb_turn, nb_ant_per_player, nb_player, minimal_nb_player, initial_energy, initial_acid)
        self.decisionMaker.set_game(self.game)

    def join_game(self, game_id):
        self.game = g.join_game(game_id)
        self.decisionMaker.set_game(self.game)

    def play_turn(self):
        if self.game is not None:
            return True
        else:
            self.strategy = self.decisionMaker.choose_strategy(self.game)
            self.short_path = self.strategy.choose_actions()
            return False

