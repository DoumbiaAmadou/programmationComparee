#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#define ERR(s) perror(s); exit(EXIT_FAILURE);
#define SIZE 640 * 1024

int compare(const void *x, const void *y);
void set_prime_numbers(char tab[]);

int main(int argc, char **argv) {
  int i = 0, index = 0;
  long long int value;  
  char t[SIZE];
  long long int sorted_primes[SIZE];
  if(argc > 1) {
    fprintf(stderr, "Usage : ./genrandom N | %s\n",argv[0]);
    exit(EXIT_FAILURE);
  } else {
    memset(sorted_primes,0,SIZE);
    set_prime_numbers(t);
    /* Reading the given value until the end */
    while(scanf("%lld",&value) == 1) {
    assert(value < SIZE);
    printf("%lld is primer ? %d\n", value, t[value]);
    /* If [value] is prime, we add into the array of prime numbers array */
    if(t[value]) {
      sorted_primes[index++] = value;      
    }
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

/* [set_prime_number tab] keeps only the prime number of the array [tab]. */
void set_prime_numbers(char tab[]) {
  unsigned long long int i = 0 , j = 0;
  memset(tab,1, SIZE);
  for(i = 2; i < SIZE; i++) {
    if(tab[i]) {
      for(j = i; i * j < SIZE; j++) {
	tab[i * j] = 0;
      }
    }
  }
}

/* [compare x y] test if [x] is lesser than [y]. */
int compare(const void *x, const void *y) {
  const long long int *dx= (const long long int*) x;
  const long long int *dy= (const long long int*) y;
  return (*dx > *dy);
}



