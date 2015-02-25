#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZE 640000

/***************************************************
 * principe : nous avons un tableau de booléen
 * nous éléminnons à chaques nombres premier trouvés
 * les multiples de ce nombre
 * *************************************************/

void eratosthene(int N){
	printf("1\n");//cas particulier 1
	char prem[SIZE];
	memset(prem,1,SIZE);
	int i,j,cpt = 1;
	for(i=2 ; (i<SIZE) && (cpt<N) ; i++){

		if(prem[i]){
			
			for(j=i*2 ; j<SIZE ; j+=i)
				prem[j]=0;

			cpt++;
			printf("%d\n",i);
		}

	}
}


int main(int argc,char *argv[]){
	if(argc != 2) return EXIT_FAILURE;
	eratosthene(atoi(argv[1]));
	return EXIT_SUCCESS;
}
