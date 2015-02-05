//
//  main.c
//  malloc
//
//  Created by Vladislav Fitc on 05/02/15.
//  Copyright (c) 2015 Fitc. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define MEMSIZE 1024

typedef struct _mem_dictionary
{
    void *addr;
    size_t size;
} mem_dictionary;


mem_dictionary *dictionary = NULL;
int dictionary_ct = 0;

void *mymalloc(size_t size)
{
    void *return_ptr = sbrk(size);
    
    if (dictionary == NULL)
        dictionary = sbrk(MEMSIZE * sizeof(mem_dictionary));
    
    dictionary[dictionary_ct].addr = return_ptr;
    dictionary[dictionary_ct].size = size;
    dictionary_ct++;
    
    return return_ptr;
}

void myfree(){
    dictionary_ct--;
}

int main(int argc, const char * argv[]) {
}