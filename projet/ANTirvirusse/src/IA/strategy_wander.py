 # -*- coding: utf-8 -*-

from . import strategy

#Strategy d'exploration de map pour découvrir des ressources
class StrategyWander(strategy.Strategy):
    def choose_actions(self):
        for ant in self.world.ants:
            self.action_for_ant(ant)

    def action_for_ant(self,ant):
        pass