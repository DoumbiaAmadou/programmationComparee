from .. import game as g
from abc import ABCMeta, abstractmethod
from . import analyst

#The class Strategy is the metaclass of the strategies that can be applied to play a turn. It takes a world as parameter
#and will decide what to play depending on the state of this world.
class Strategy():
    __metaclass__ = ABCMeta

    def __init__(self, world):
        self.world = world
        self.short_path=None
        self.analyst=analyst.Analyst()

    @abstractmethod
    def choose_actions(self):
        pass

    #The pathfinding algorithm use the A* algorithm. To avoid recalculating it at each turn, we keep the one calculated on
    #the last turn, so if an and still want to go to the same point, we don't have to compute the shortest path again.
    def set_short_path(self,sp):
        self.short_path=sp