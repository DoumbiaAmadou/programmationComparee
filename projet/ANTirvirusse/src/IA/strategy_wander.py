 # -*- coding: utf-8 -*-

from . import strategy

#Strategy d'exploration de map pour découvrir des ressources
class StrategyWander(strategy.Strategy):
    def choose_actions(self):
        for ant in self.world.ants:
            self.action_for_ant(ant)

    def action_for_ant(self,ant):
        ennemies=self.analyst.enemies_near(ant.x,ant.y,8)
        if len(ennemies)!=0:
            self.fight(ant,ant.x,ant.y) #recherche automatiquement les ennemis proches et les attache
        else:
            resource=self.analyst.nearest_resource()
            if (self.analyst.distance(ant.x,ant.y,resource.x, resource.y))<8:
                self.go_to(ant,resource.x,resource.y)