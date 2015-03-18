from .. import game as g
from abc import ABCMeta, abstractmethod


class Strategy():
    __metaclass__ = ABCMeta

    def __init__(self, game):
        self.game = game
        self.short_path=None

    @abstractmethod
    def choose_actions(self):
        pass

    def set_short_path(self,sp):
        self.short_path=sp