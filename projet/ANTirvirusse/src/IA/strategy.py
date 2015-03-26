# -*- coding: UTF-8 -*-

from abc import ABCMeta, abstractmethod
from . import analyst

# The class Strategy is the metaclass of the strategies that can be applied to play a turn. It takes a world as parameter
# and will decide what to play depending on the state of this world.
class Strategy():
    __metaclass__ = ABCMeta

    def __init__(self, world):
        self.world = world
        self.short_path = None
        self.analyst = analyst.Analyst()

    @abstractmethod
    def choose_actions(self):
        pass

    def go_to(self, ant, p):  # envoie la fourmi au point p
        came_from, cost_so_far = self.analyst.best_path(ant.x, ant.y, p.x, p.y)
        next_case = (p.x, p.y)
        while next_case.x - ant.x + next_case.y + ant.y > 1:  # Pour aller au point p, on cherche la prochaine case
            if next_case.x - ant.x + next_case.y + ant.y != 0:
                next_case = came_from[(next_case.x, next_case.y)]
        next_relative = (
            next_case.x - ant.x, next_case.y - ant.y)  # 1 si cause de gauche ou en haut, -1 si case de droite ou en bas
        o = ant.get_orientation()
        if next_relative.x == o[1] and next_relative.y == o[2]:  # Si bonne direction
            ant.forward()
        else:
            self.turn_ant(ant, next_relative)

    def fight(self, ant, x, y):
        ennemies = self.analyst().enemies_near(x, y, 8)
        closer = None
        for en in ennemies:
            if closer == None:
                closer = en
            elif en.x - ant.x + en.y - ant.y < closer.x - ant.x + closer.y - ant.y:
                closer = en

        if closer.x - ant.x + closer.y - ant.y == 1:
            o = ant.get_orientation()
            if closer.x == o[1] and closer.y == o[2]:  # Si bonne direction
                ant.attack(ant.get_acid())
            else:
                self.turn_ant(ant, closer)


    # fait se tourner une fourmi vers la case case
    def turn_ant(self, ant, case):
        o = ant.get_orientation()
        if ( (-1 * case.x) + case.y) * (o[1] + o[2]) > 0:  # si case vers la gauche
            # (inversion de l'axe de next et multiplication)
            # simplification de l'algorithme pour trouver la direction
            ant.left()
        else:
            ant.right()




