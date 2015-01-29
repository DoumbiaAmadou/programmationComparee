//
//  main.c
//  Ed
//
//  Created by Vladislav Fitc on 29/01/15.
//  Copyright (c) 2015 Fitc. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>

#define NUMBER_OF_STRINGS   100
#define STRING_LENGTH       80
#define EXIT                69


char text[NUMBER_OF_STRINGS][STRING_LENGTH+1];

void insert(int m){
    
}

void delete(int m, int n){
    
}

void replace(int m, int n){
    
}

void showText(){
    
}

int main(int argc, const char * argv[]) {
    while (1) {
        int m, n;
        char str[128];
        fgets(str, 126, stdin);
        
        if (sscanf(str, "I %d", &m ) == 1) {
            insert(m);
            continue;
        }
        
        if (sscanf(str, "D %d %d", &m, &n ) == 2) {
            delete(m, n);
            continue;
        }
        
        if (sscanf(str, "R %d %d", &m, &n ) == 2) {
            replace(m, n);
            continue;
        }
        
        if (str[0] == EXIT) {
            exit(EXIT_SUCCESS);
        }
    }
    return 0;
}
