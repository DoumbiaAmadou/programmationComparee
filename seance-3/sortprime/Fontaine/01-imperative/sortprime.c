#include <stdio.h>
#include <string.h>
#include <assert.h>

#define MEM_SIZE (640*1024)
#define MAX_NUM ((MEM_SIZE)/(sizeof (unsigned char)))

/**
 * This represents both the Sieve of Eratosthene and an array of counters for
 * each number.
 *
 * When the program starts, all the array is filled with 1's. These 1's are
 * changed to 0's when we run the Sieve algorithm. When we find a prime number,
 * we increment the corresponding index.
 *
 * This means the program can process numbers up to MAX_NUM-1 (655,359), with
 * max. 254 times the same number.
 *
 * Most of this array will be set to 0s.
 **/
static unsigned char numbers[MAX_NUM];

/**
 * Set all [numbers[i]] to 0 for all [i] where [i%n == 0].
 **/
void eratosthene(int n) {
        if (n < 2) { return; }
        for (int i=n*2; i<MAX_NUM; i += n) {
                numbers[i] = 0;
        }
}

/**
 * Initialize the Sieve of Eratosthene
 **/
void init(void) {
        memset(numbers, 1, MAX_NUM);
}

/**
 * Add a number to our counters
 **/
void add_number(const unsigned int n) {
        if (numbers[n]) {
                printf("%d\n", n);
                assert(numbers[n]++ < 254);
        }
}

/**
 * Print the list of collected numbers
 **/
void print_list(void) {
        for (int i=0; i<MAX_NUM; ++i) {
                for (int j=1; j<numbers[i]; ++j) {
                        printf("%d\n", i);
                }
        }
}

int main(int argc, char **argv) {
        if (argc != 2) {
                fprintf(stderr, "Usage:\n\tgenrandom <k> | %s <k>\n", argv[0]);
                return 1;
        }

        init();

        int line = 1,
            n = 0;

        while (scanf("%d\n", &n) == 1) {
                assert(n < MAX_NUM);
                eratosthene(line);
                add_number(n);
                ++line;
        }

        print_list();

        return 0;
}
