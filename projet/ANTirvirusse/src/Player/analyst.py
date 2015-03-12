# -*- coding: utf-8 -*-

import math
import heapq


class Analyst():
    acid_range = 6

    def __init__(self, game):
        self.game = game

    def is_under_control(self, x, y):
        enemy = self.enemy_ants_at_fire_range(x, y)
        allies = self.ally_ants_at_fire_range(x, y)
        if len(enemy) > len(allies):
            return False
        else:
            acid_count = 0
            for ally in allies:
                acid_count += ally.get_acid
            if acid_count < len(enemy) * self.game.get_initial_energy():
                return False
            else:
                return True

    def time_to_go(self, x_from, y_from, x_to, y_to):
        pass

    def distance(self, x_from, y_from, x_to, y_to):
        dx = x_from - x_to
        dy = y_from - y_to
        return math.sqrt(dx * dx + dy * dy)

    def ally_ants_around(self, x, y, radius):
        return self.nb_ant_at_distance(x, y, radius, self.game.ants, lambda: self.time_to_go)

    def ally_ants_at_fire_range(self, x, y):
        return self.nb_ant_at_distance(x, y, self.acid_range, self.game.ants, lambda: self.distance)

    def enemy_ants_around(self, x, y, radius):
        enemy_ants = []
        return self.nb_ant_at_distance(x, y, radius, enemy_ants, lambda: self.time_to_go)

    def enemy_ants_at_fire_range(self, x, y):
        enemy_ants = []
        return self.nb_ant_at_distance(x, y, self.acid_range, enemy_ants, lambda: self.distance)

    def nb_ant_at_distance(self, x, y, radius, ant_array, distance_calculus):
        res = []
        for ant in ant_array:
            ax, ay = ant.get_position()
            if distance_calculus(ax, ay, x, y) <= radius:
                res.append(ant)
        return res

    def nearest_resource(self, resource_type, x, y):
        for case in self.game.get_map():
            if case.is_food():
                pass

    def nearest_wheat(self, x, y):
        self.nearest_resource("wheat", x, y)

    def nearest_sugar(self, x, y):
        self.nearest_resource("cheese", x, y)

    def nearest_cheese(self, x, y):
        self.nearest_resource("sugar", x, y)

    # a_star_search from redblobgames.com : path finding avec A*, la fonction d'heuristique est ici la distance de
    # manhattan. L'algorithme est modifié pour prendre en compte le temps de tourner.
    def best_path(self, x_from, y_from, x_to, y_to):
        queue = PriorityQueue()
        start = (x_from, y_from)
        goal = (x_to, y_to)
        queue.put(start, 0)
        came_from = dict
        cost_so_far = dict
        came_from[start] = None
        cost_so_far[start] = 0
        found = False

        while not queue.empty() and not found:
            current = queue.get()

            if current == goal:
                found = True

            for next_case in self.game.get_map().neighbors(current):
                new_cost = cost_so_far[current] + self.game.get_map().cost(current, next_case)
                if next_case not in cost_so_far or new_cost < cost_so_far[next_case]:
                    cost_so_far[next_case] = new_cost
                    priority = new_cost + self.manhattan(goal, next_case)
                    if self.direction_change(came_from[current], current, next_case):
                        priority += 1
                    queue.put(next_case, priority)
                    came_from[next_case] = current

        return came_from, cost_so_far

    def manhattan(self, a, b):
        (x1, y1) = a
        (x2, y2) = b
        return abs(x1 - x2) + abs(y1 - y2)

    def direction_change(self, case_from, case_middle, case_to):
        if self.same_sign(case_middle[0] - case_from[0], case_to[0] - case_middle[0]) and self.same_sign(
                        case_middle[1] - case_from[1], case_to[1] - case_middle[1]):
            return False
        else:
            return True

    def same_sign(self, a, b):
        if (a > 0 and b > 0) or (a == 0 and b == 0) or (a < 0 and b < 0):
            return True
        else:
            return False


# queue pour l'algorithme de path finding A*
class PriorityQueue:
    def __init__(self):
        self.elements = []

    def empty(self):
        return len(self.elements) == 0

    def put(self, item, priority):
        heapq.heappush(self.elements, (priority, item))

    def get(self):
        return heapq.heappop(self.elements)[1]