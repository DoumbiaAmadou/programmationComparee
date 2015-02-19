#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define MEM_SIZE (640*1024)
#define BUF_SIZE 1024

#define MAX_NUM ((MEM_SIZE)-(BUF_SIZE)/2)

char numbers[MAX_NUM],
     counts[MAX_NUM];

void eratosthene(int n) {
        if (n <= 1) return;

        for (int i=n*2; i<MAX_NUM; i += n) {
                numbers[n] = 0;
        }
}

void init(void) {
        for (int i=0; i<MAX_NUM; ++i) {
                numbers[i] = 1;
                counts[i] = 0;
        }
}

void add_number(int n) {
        if (numbers[n]) { ++counts[n]; }
}

void print_list(void) {
        for (int i=0; i<MAX_NUM; ++i) {
                for (int j=0; j<counts[i]; ++j) {
                        printf("%d\n", numbers[i]);
                }
        }
}

int main(int argc, char **argv) {
        if (argc != 2) {
                fprintf(stderr, "Usage:\n\tgenrandom <k> | %s <k>\n", argv[0]);
                exit(1);
        }

        init();

        int line = 0,
            n,
            ret;

        while ((ret = scanf("%d\n", &n)) == 1) {
                eratosthene(line);
                add_number(n);
                line++;
        }

        print_list();

        return 0;
}
