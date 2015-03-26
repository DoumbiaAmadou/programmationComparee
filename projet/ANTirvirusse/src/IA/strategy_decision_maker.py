from . import strategy_wander, analyst, strategy_hold

#StrategyDecisionMaker will analyse a given world from the model, and choose the most adapted strategy depending on the
#state of the world.
class StrategyDecisionMaker():
    def __init__(self, world):
        self.resources_controlled = []
        self.ants = []
        self.analyst=analyst.Analyst(world)
        self.world=world

    #Chose the most adapted strategy for a given world.
    #It currently has only two possible strategies : wander, to discover the map, and hold, ton hold the ressources
    def choose_strategy(self):
        if len(self.analyst.resources_under_control())> len(self.world.get_ants())/3.0:
            res = strategy_hold.StrategyHold(self.world)
        else :
            res=strategy_wander.StrategyWander(self.world)
        return res
