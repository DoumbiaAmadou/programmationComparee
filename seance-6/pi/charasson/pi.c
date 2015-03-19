#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <limits.h>
#include <gmp.h>

double xy(){
	double x = rand()/ (double)RAND_MAX;
	double y = rand()/ (double)RAND_MAX;
	return x*x+y*y;
}

int main(){
	srand(time(NULL));
	//printf("r = %1.36f\n",r());
	unsigned long long n= 0, k=0;
	for(n=0;n<INT_MAX;n++){
		if(xy()<=1)
			k++;
		//printf("%lld %lld\n",k,n );
	}

	//printf("pi = %1.36f\n",((double)k/(double)n)*4 );
	printf("%lld %lld\n",k,n );

	return 0;
}
