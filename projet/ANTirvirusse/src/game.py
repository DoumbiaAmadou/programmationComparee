 # -*- coding: utf-8 -*-

from dataObject import *
import network_layer as nl
import pprint

class Player(object):

    def __init__(self, login, password):
        self.games = []

    @classmethod
    def login(cls, login, password):
        if nl.login(login, password):
            return Player(login, password)
        else:
            return None

    @classmethod
    def register(cls, login, password):
        if nl.register_user(login, password):
            return Player(login, password)
        if nl.login("vlad", "muravei"):
            return None

    def get_created_games():
        my_games = filter(lambda game: game["game_description"]["creator"] == "vlad", nl.get_games())
        my_games_ids = map(lambda game: game["game_description"]["identifier"], my_games)
        return my_games_ids

    def destroy_game(self, game_id):
        nl.game_destroy(game_id)

    def destroy_all_games(self):
        for game_id in get_created_games:
            self.destroy_game(game_id)

    def create_game(self, pace, nb_turn, nb_ant_per_player, nb_player, minimal_nb_player, initial_energy, initial_acid, users='+', teaser=""):
        game_id = nl.game_create(pace, nb_turn, nb_ant_per_player, nb_player, minimal_nb_player, initial_energy, initial_acid, users, teaser)
        if game_id is not None:
            status = nl.game_status(game_id)
            game_state = status["status"]["status"]
            initial_energy = status["initial_energy"]
            nb_ant_per_player = status["nb_ant_per_player"]
            initial_acid = status["initial_acid"]
            pace = status["pace"]
            game = Game(game_id, nb_ant_per_player, initial_acid, initial_energy, pace)
            self.games.append(game)
            return game
        else:
            return None

    def join_game(self, game_id):
        status = nl.game_status(game_id)
        game_state = status["status"]["status"]
        if game_state != "playing":
            print "Impossible to join game. Current status: %s" %(game_state)
            return None
        else:
            nb_ant_per_player = status["nb_ant_per_player"]
            initial_acid = status["initial_acid"]
            initial_energy = status["initial_energy"]
            pace = status["pace"]
            game = Game(game_id, nb_ant_per_player, initial_acid, initial_energy, pace)
            self.games.append(game)
            return game


class Game():

    @classmethod
    def state_of_game(cls, game_id):
        print nl.game_status(game_id)

    def __init__(self, gid, nb_ants, initial_acid, initial_energy, pace):
        nl.game_join(gid)
        self.gid = gid
        self.pace = pace
        self.ants = []
        self.game_map = {}
        self.initial_acid=initial_acid
        self.initial_energy=initial_energy

        for ant_id in range(0, nb_ants):
            ant = Ant(ant_id, initial_energy, initial_acid, True)
            self.ants.append(ant)

        self.make_move()

    def get_initial_acid(self):
        return self.initial_acid

    def get_initial_energy(self):
        return self.initial_energy

    def get_map(self):
        return self.game_map

    def destroy(self):
        nl.game_destroy(self.gid)

    def make_move(self):
        commands = map(lambda ant: AttachedCommand(ant.ident, ant.next_command), self.ants)
        ants_stats = game_play(self.gid, commands)

        for stat in ants_stats:
            ant_state = stat[0]
            ant_id = ant_state["id"]
            current_ant = filter(lambda ant: ant.ident == ant_id, self.ants)[0]

            if current_ant is not None:
                current_ant.set_energy(int(ant_state["energy"]))
                current_ant.set_acid(int(ant_state["acid"]))
                current_ant.set_brain(ant_state["brain"]=="controlled")
                current_ant.set_position(int(ant_state["x"]), int(ant_state["y"]))

            cases = map(lambda case_dict: Case(case_dict["content"]["kind"], int(case_dict["x"]), int(case_dict["y"])), stat[1])
            for case in cases:
                self.game_map[(case.x, case.y)] = case

        for ant in self.ants:
            print ant

        for key, value in self.game_map.iteritems():
            print "%s \t: %s" %(key, value)

    def show_status(self):
        print nl.game_status(self.gid)

def test():
    player = Player.login("vlad", "muravei")
    if player is not None:
        game = player.create_game(teaser='Test',users='vlad',pace=50, nb_turn=100, nb_ant_per_player=3, nb_player=2, minimal_nb_player=1, initial_energy=100, initial_acid=50)
        game.show_status()
        game.destroy()

test()
