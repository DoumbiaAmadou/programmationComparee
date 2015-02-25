#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#define ERR(s) perror(s); exit(EXIT_FAILURE);
#define SIZE 640 * 1024

int compare(const void *x, const void *y);
int is_prime(char **tab, int value);

int main(int argc, char **argv) {
  int i = 0, index = 0, bool;
  long long int value;  
  char *t;
  long long int sorted_primes[SIZE];
  if(argc > 1) {
    fprintf(stderr, "Usage : ./genrandom N | %s\n",argv[0]);
    exit(EXIT_FAILURE);
  } else {
    memset(sorted_primes,0,SIZE);
    /* Reading the given value until the end */
    while(scanf("%lld",&value) == 1) {
      assert(value < SIZE);
      bool = is_prime(&t,value);
      printf("%lld is primer ? %d\n", value,bool);
      /* If [value] is prime, we add into the array of prime numbers array */
      if(bool) {
	sorted_primes[index++] = value;      
      }
      free(t);
    }
    /* We use the quick sort algorithm to sort our array of sorted prime 
       numbers. No need to sort the all values in the array, only these we met 
       before.*/
    qsort(&sorted_primes, index, sizeof(long long int), compare);
    printf("The list of all prime numbers that we met : ");
    for(i = 0; i < index; i++) {
      printf("%lld ", sorted_primes[i]);
    }
    return 0;
  }
}

/* [is_prime tab value] check if [value] is a prime number */
int is_prime(char **tab, int value) {  
  unsigned long long int i = 0 , j = 0;
  (*tab) = (char *)malloc(sizeof(char) * (value));
  if(tab == NULL) {
    ERR("Error malloc");
  }
  memset((*tab),1,value+1);
  for(i = 2; i <= value; i++) {
    if((*tab)[i]) {
      for(j = i; i * j <= value; j++) {
	(*tab)[i * j] = 0;
      }
    }
  }
  return (*tab)[value];
}

/* [compare x y] test if [x] is lesser than [y]. */
int compare(const void *x, const void *y) {
  const long long int *dx= (const long long int*) x;
  const long long int *dy= (const long long int*) y;
  return (*dx > *dy);
}



