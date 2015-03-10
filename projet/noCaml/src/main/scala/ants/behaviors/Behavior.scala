package ants.behaviors

/* L'IA des fourmies pourra utiliser un "behavior" pour determiner
 * sa prochaine action.
 */
abstract class Behavior (val priorities : List[String])

/* Exemple de comportement possible (A approfondir) */
class FoodSeeker extends Behavior (List("Food"))