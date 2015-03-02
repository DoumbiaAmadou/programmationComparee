#include <stdio.h>

#define SIZE 640*1024

char crible[SIZE];
int number;

void eratosthene() {
  int i,j;
  for(i = 2; i <= SIZE; i++) {
    if(crible[i]) {
      for(j = i + i; j < SIZE; j+=i) {
	crible[j] = 0;
      }  
    }
  }
}

int main() {
  crible[0] = 0;
  crible[1] = 0;
  int i;
  for(i = 2; i < SIZE; i++) {
    crible[i] = 1;
  }
  eratosthene();
  while(scanf("%d", &number) != EOF) {
    if(crible[number]) {
      printf("%d : premier\n",number);
    } else {
      printf("%d : non premier\n",number);
    }
  }
  return 0;
}
