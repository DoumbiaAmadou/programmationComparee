#define _BSD_SOURCE

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

void *my_malloc(int size) {
    void *retour = NULL;
    retour = sbrk(size * sizeof(int));
    if(retour == (void *) - 1) {
        printf("Error alloc\n");
        return NULL;
    }
    return retour;
}

int main(int argc, char** argv) {
    int i;
    int *tab;
    int size = 1024;
    if(argc >= 2) {
        size = atoi(argv[1]);
        if(size <= 0) size = 1024;
    }
    tab = (int*) my_malloc(size * sizeof(int));
    if(argc >= 3) {
        tab[0] = atoi(argv[2]);
    }
    for(i=1; i<size; i++) tab[i] = tab[i-1] + 1;
    return EXIT_SUCCESS;
}
