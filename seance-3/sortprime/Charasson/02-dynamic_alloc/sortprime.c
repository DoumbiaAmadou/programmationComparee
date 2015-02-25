#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZE 640000

char* prem;

void eratosthene(){
	memset(prem,1,SIZE);
	prem[0]=0;
	int i,j;
	for(i=2 ; (i<SIZE) ; i++){
		if(prem[i]){
			for(j=i*2 ; j<SIZE ; j+=i)
				prem[j]=0;
		}
	}
}


int main(){
	int num = 0;
	prem = (char*) malloc(sizeof(char)*SIZE);
	eratosthene();
	
	/* afficher le plus vite possible le nombre si il est premier */
	while(fscanf(stdin,"%d\n",&num)!=EOF){
		if(num<SIZE){
			if(prem[num]){
				printf("%d\n",num);
				prem[num]=2;/* petite optimisation */
			}
			/* else printf("%d is not prime\n",num); */
		}
	}

	/* afficher la liste des nombres premiers rencontrés */
	printf("sorted prime list :\n"); 
	for(num=0 ; num<SIZE ; num++){
		if(prem[num] == 2)
			printf("%d\n",num);
	}
	free(prem);

	return EXIT_SUCCESS;
}
