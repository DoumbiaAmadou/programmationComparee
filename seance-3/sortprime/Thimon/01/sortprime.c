#include <stdio.h>
#include <stdlib.h>

#define TAILLE 640 * 1024
int nbPremiers[TAILLE];

void initTableau()
{
    int i;
    nbPremiers[0] = 0;
    nbPremiers[1] = 0;
    for (i = 2; i < TAILLE; i++)
        nbPremiers[i] = 1;
}

void cribleEratosthene()
{
    initTableau();

    int i,j;
    for (i = 2; i < TAILLE; i++)
        for(j = i * 2 ; j < TAILLE ; j = j + i)
            nbPremiers[j] = 0;
}
void nbPremierRencontre(int chiffre)
{
    nbPremiers[chiffre] = -1;
}
void afficheNbRencontres()
{
    int i;
    for (i = 2; i < TAILLE; i++)
        if(nbPremiers[i] == -1)
            printf("%d\t",i);
}

int main()
{
    int i = 0;
    int chiffre;
    int arret = 10;
    cribleEratosthene();

    while(i < arret)
    {
        printf("Entrer un chiffre:\n");
        scanf("%d",&chiffre);
        if(nbPremiers[chiffre] == 1)
        {
            printf("%d\n",chiffre);
            nbPremierRencontre(chiffre);
        }
        i++;
    }

    afficheNbRencontres();
    return (EXIT_SUCCESS);
}

