Le programme est effectué en java pour des raisons de connaissances du langage.

Il fait environ 90 lignes (en comptant les espaces)

Chaque mot dans le dictionnaire est stocké dans un sous dictionnaire correspondant à sa taille sous la forme d'un Element, qui contient le mot en brute, et sous sa forme triée alphabétiquement.
Lorsqu'un mot est recherché, le sous dictionnaire de taille correspondante est parcouru, avec une recherche sur le mot trié. Ceci permet d'améliorer grandement le temps de comparaison.


La complexité de la recherche est de l'ordre O(n).

