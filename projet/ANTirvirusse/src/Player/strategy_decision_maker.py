from .. import game as g
from . import strategy_basic


class StrategyDecisionMaker():
    def __init__(self):
        self.game = None
        self.resources_controlled = []
        self.ants = []

    def choose_strategy(self, world):
        res = strategy_basic.StrategyBasic(self.game)
        return res

    def set_game(self, game):
        self.game = game