#include <stdio.h>

#define SIZE 640*1024

int max = -1, current=-1;
char primes[SIZE];

void init(){
  int i;
  for(i=2;i<SIZE;i++)
    primes[i]=1;
  primes[0] = primes[1] = 0;
  max = 1;
}

void erato(){
  int i,j;
  for(i=max+1; i<=current;i++){
    for(j=i*2; j<SIZE; j+=i)
      primes[j] = 0;
  }
}

int main(){
  init();
  while(scanf("%d",&current)!=EOF){
    if(current > max){
      erato();
      max = current;
    }
    printf("%d : %s\n",current,primes[current]?"Prime":"Not prime");
  }
}

