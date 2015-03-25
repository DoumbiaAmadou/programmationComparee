from . import strategy

#Strategy d'exploration de map pour découvrir des ressources
class StrategyWander(strategy.Strategy):
    def choose_actions(self):
        short_path = []
        for ant in self.world.ants:
            self.action_for_ant(ant)
        return short_path

    def action_for_ant(self,ant):
        pass