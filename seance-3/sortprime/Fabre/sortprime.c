#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZE 640000
#define SQSIZE 800

/*
 * To compute memory usage:
 * 1- Compile wuth -g flag
 * 2- Run in gdb (don't forget to "< input")
 * 3- Add a breakpoint at end of main
 * 4- Run functions "malloc_stats" and "malloc_info" by running :
 *       i- call malloc_stats()
 *       ii- call malloc_info(0, stdout)
 */

/* In order to respect the exercise :
 */

int mem[SIZE/4];

#define VARA (mem[0])
#define VARB (mem[1])

int main(int argc, char** argv) {
    /*
     * To get results with gdb :
     * int *mem = (int*) malloc((SIZE / 4) * sizeof(int));
     */
    memset(mem, 1, (SIZE / 4) * sizeof(int));
    for(VARA = 2; VARA < (SQSIZE / 4); VARA++) {
        if(mem[VARA]) {
            for(VARB = VARA * VARA; VARB < (SIZE / 4); VARB += VARA) {
                mem[VARB] = 0;
            }
        }
    }
    while(scanf("%d", &VARA) != EOF) {
        if(mem[VARA]) mem[VARA] = 2;
    }
    for(VARA = 2; VARA < (SIZE / 4); VARA++) {
        if(mem[VARA] == 2) printf("%d\n", VARA);
    }
    return EXIT_SUCCESS;
}

/*
 *
 * gdb results :
 * (gdb) call malloc_stats()
 * Arena 0:
 * system bytes     =          0
 * in use bytes     =          0
 * Total (incl. mmap):
 * system bytes     =     643072
 * in use bytes     =     643072
 * max mmap regions =          1
 * max mmap bytes   =     643072
 * $1 = -136494656
 *
 *
 *
 * (gdb) call malloc_info(0, stdout)
 * <malloc version="1">
 * <heap nr="0">
 * <sizes>
 * </sizes>
 * <total type="fast" count="0" size="0"/>
 * <total type="rest" count="0" size="0"/>
 * <system type="current" size="0"/>
 * <system type="max" size="0"/>
 * <aspace type="total" size="0"/>
 * <aspace type="mprotect" size="0"/>
 * </heap>
 * <total type="fast" count="0" size="0"/>
 * <total type="rest" count="0" size="0"/>
 * <system type="current" size="0"/>
 * <system type="max" size="0"/>
 * <aspace type="total" size="0"/>
 * <aspace type="mprotect" size="0"/>
 * </malloc>
 * $2 = 0
 */
