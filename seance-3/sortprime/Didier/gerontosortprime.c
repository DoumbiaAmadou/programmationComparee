#include <stdlib.h>
#include <stdio.h>

int max = -1, current = -1;
char * primes;

void init(){
  primes = malloc(2*sizeof(char));
  primes[0] = primes[1] = 0;
  max = 1;
}

void expand(){
  int i;
  primes = realloc(primes,current+1);
  for(i=max+1;i<=current;i++)
    primes[i]=1;
}

void erato(){
  int i,j;
  for(i=max+1; i<=current;i++){
    for(j=i*2; j<=current; j+=i)
      primes[j] = 0;
  }
  max = current;
}

int main(){
  init();
  while(1){
    int c = scanf("%d",&current);
    if(c==EOF || !c) break;
    if(current > max){
      expand();
      erato();
      max = current;
    }
    printf("%d : %s\n",current,primes[current]?"Prime":"Not prime");
  }
  int i;
  for(i=0; i<=max; i++)
    if(primes[i]) printf("%d\n",i);
  return 0;
}

