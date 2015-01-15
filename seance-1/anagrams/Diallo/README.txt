-Langage utilisé : scala

-Compiler : lancer le fichier compile.sh, puis entrez "scala Anagram mot1 mot2 ..."

-Nombre de ligne de code : 41

-Complexité:
	
-Le programme compare la taille du mot entré en paramètre par rapport aux auxtres. Puis, en cas d'egalité, il comptabilise le nombre d'ocurrence de chaque lettre du mot et le compare avec celui des autres.
	
Avec n le nombre de lettres du mot à comparer et m(i) le nombre de lettres du mot courant i avec lequel on le compare
on a en tous dans le pire des cas:
    (n*n+(n*m(i)))*(nb de mots du dictionnaire)
