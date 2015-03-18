from . import strategy


class StrategyBasic(strategy.Strategy):
    def choose_actions(self):
        short_path = []
        for ant in self.game.ants:
            pass
        return short_path


