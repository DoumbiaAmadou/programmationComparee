# -*- coding: utf-8 -*-

from .. import game as g
from . import strategy_decision_maker

#An IAPlayer is assigned to a new game. When the network layer updates the model with the observations for this turn,
#Play turn is called
class IAPlayer(g.Player):

    def __init__(self, login, password, world):
        super(IAPlayer, self).__init__(login, password)
        self.strategy = None
        self.decisionMaker = strategy_decision_maker.StrategyDecisionMaker()
        self.world = world

    def play_turn(self):
        if self.world is not None:
            return False
        else:
            self.strategy = self.decisionMaker.choose_strategy(self.world)
            self.short_path = self.strategy.choose_actions()
            return True

