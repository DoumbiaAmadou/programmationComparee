# -*- coding: utf-8 -*-


from . import strategy

# Strategy d'exploration de map pour découvrir des ressources
class StrategyHold(strategy.Strategy):
    def choose_actions(self):
        self.resources = self.analyst.resources_under_control()  #On récupère la liste des ressources sous contrôle
        self.resources_contested={} #associe les positions des ressources contestées au nombre de fourmis parti les défendre

        sum_x=0
        sum_y=0
        nb=0

        for res in self.resources:
            sum_x+=res.x
            sum_y+=res.y
            nb+=1

            if self.analyst.enemies_near(res.x, res.y, 8):
                self.resources_contested[(res.x,res.y)]=0

        self.center={'x':sum_x/nb,'y':sum_y/nb} #point central des ressources à défendre

        for ant in self.world.ants:
            self.action_for_ant(ant)

    def action_for_ant(self, ant):
        order_given=False
        for pos in self.resources_contested.keys(): #Pour chaque ressource contestée
            en=self.analyst.enemies_near(pos[0], pos[1],8)
            if len(en)>= self.resources_contested[(pos[0], pos[1])]:    #Si le nombre de défenseurs n'est pas supérieur au nombre d'ennemis
                self.fight(ant, pos[0], pos[1]) #On envoie la fourmi se battre autour de ce point
                self.resources_contested[(pos[0], pos[1])]+=1   #et on incrémente le nombre de défenseur pour ce point
                order_given=True
        if not order_given:
            self.go_to(ant,self.center)



