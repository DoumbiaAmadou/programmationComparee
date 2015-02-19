#include <stdio.h>
#include <string.h>
#include <assert.h>

#define MEM_SIZE (640*1024)
#define BUF_SIZE 1024

#define MAX_NUM ((MEM_SIZE)-(BUF_SIZE)/2)

// we could optimize the memory usage here but the program would be slower
static unsigned char numbers[MAX_NUM],
                     counts[MAX_NUM];

void eratosthene(int n) {
        if (n < 2) { return; }
        for (int i=n*2; i<MAX_NUM; i += n) {
                numbers[i] = 0;
        }
}

void init(void) {
        memset(numbers, 1, MAX_NUM);
        memset(counts, 0, MAX_NUM);
}

void add_number(const int n) {
        if (numbers[n]) {
                printf("%d\n", n);
                ++counts[n];
        }
}

void print_list(void) {
        for (int i=0; i<MAX_NUM; ++i) {
                for (int j=0; j<counts[i]; ++j) {
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
