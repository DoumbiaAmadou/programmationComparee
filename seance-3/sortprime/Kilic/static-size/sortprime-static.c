#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#define ERR(s) perror(s); exit(EXIT_FAILURE);
#define SIZE 640 * 1024

void set_prime_numbers(char **tab);

int main(int argc, char **argv) {
  long long int value;
  char *t;
  set_prime_numbers(&t);
  while(scanf("%lld",&value) == 1) {
    assert(value < SIZE);
    printf("%lld is primer ? %d\n", value, t[value]);
  }
  free(t);
  return 0;
}


void set_prime_numbers(char **tab) {
  unsigned long long int i = 0 , j = 0;
  (*tab) = (char *)malloc(sizeof(char) * SIZE);
  if((*tab) == NULL) {
    ERR("Error malloc");    
  }
  memset((*tab),1, SIZE);
  for(i = 2; i < SIZE; i++) {
    if((*tab)[i]) {
      for(j = i; i * j < SIZE; j++) {
	(*tab)[i * j] = 0;
      }
    }
  }
}


