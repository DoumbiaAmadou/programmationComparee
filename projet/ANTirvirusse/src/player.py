 # -*- coding: utf-8 -*-

import network_layer as nl
import antcommand as ac
from game import Game

class Player(object):

    def __init__(self, login, password):
        self.games = []

    @classmethod
    def whoami(cls):
        print nl.whoami()

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
        if nl.login(login, password):
            return None

    def logout(self):
        nl.logout()

    def get_created_games():
        my_games = filter(lambda game: game["game_description"]["creator"] == "vlad", nl.get_games())
        my_games_ids = map(lambda game: game["game_description"]["identifier"], my_games)
        return my_games_ids

    def destroy_game(self, game_id):
        nl.game_destroy(game_id)

    def destroy_all_games(self):
        for game_id in get_created_games:
            self.destroy_game(game_id)

    def create_game(self, pace, nb_turn, nb_ant_per_player, nb_player, minimal_nb_player, initial_energy, initial_acid, users='+', teaser="", save=False):
        game_id = nl.game_create(pace, nb_turn, nb_ant_per_player, nb_player, minimal_nb_player, initial_energy, initial_acid, users, teaser)
        if game_id is not None:
            status = nl.game_status(game_id)
            game_state = status["status"]["status"]
            initial_energy = status["initial_energy"]
            nb_ant_per_player = status["nb_ant_per_player"]
            initial_acid = status["initial_acid"]
            pace = status["pace"]
            game = Game(game_id, nb_ant_per_player, initial_acid, initial_energy, pace, save)
            self.games.append(game)
            return game
        else:
            return None

    def join_game(self, game_id, save=False):
        status = nl.game_status(game_id)
        game_state = status["status"]["status"]
        if game_state != "playing":
            print "Impossible to join game. Current status: %s" %(game_state)
            return None
        else:
            nb_ant_per_player   = status["nb_ant_per_player"]
            initial_acid        = status["initial_acid"]
            initial_energy      = status["initial_energy"]
            pace                = status["pace"]

            game = Game(game_id, nb_ant_per_player, initial_acid, initial_energy, pace, save)
            self.games.append(game)

            return game

def test():
    player = Player.login("vlad", "muravei")
    if player is not None:
        game = player.create_game(teaser='Test',users='vlad',pace=50, nb_turn=10000, nb_ant_per_player=3, nb_player=2, minimal_nb_player=1, initial_energy=100, initial_acid=50)
        while True:
            for ant in game.game_map.ants:
                print "Select command for ant %i" %(ant.ant_id)
                print "0) Forward"
                print "1) Left"
                print "2) Right"  
                print "3) Rest"
                print "4) Exit game"
                # try:
                command = input()
                if   command == 0:
                    ant.next_command = ac.Forward() 
                elif command == 1:
                    ant.next_command = ac.Left()
                elif command == 2:
                    ant.next_command = ac.Right()
                elif command == 3:
                    ant.next_command = ac.Rest()
                else:
                    game.destroy() 
                    exit()
                # except:
                    # print "Wrong command!"
                    # continue

            game.make_move()

test()