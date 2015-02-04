//
//  main.c
//  Ed
//
//  Created by Vladislav Fitc on 29/01/15.
//  Copyright (c) 2015 Fitc. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUMBER_OF_STRINGS   100
#define STRING_LENGTH       80
#define EXIT                69


char text[NUMBER_OF_STRINGS][STRING_LENGTH+1];
char outputText[NUMBER_OF_STRINGS][STRING_LENGTH+1];
int m, n, l = 0, yl = 0;
char ln[STRING_LENGTH+1];


void insert(int m, char *inputLine){
    printf("Insert on %i line: %s\n", m, inputLine);
}

void delete(int m, int n){
    printf("Delete lines from %i to %i\n", m, n);
}

void replace(int m, int n, char *inputLine){
    printf("Replace lines from %i to %i by: %s\n", m, n, inputLine);
}

void divideByStringLength(char *p){
    for (int i = 0; i < NUMBER_OF_STRINGS; ++i) {
        char *s = strncpy(text[i], &p[i*STRING_LENGTH], STRING_LENGTH);
        if (strlen(s) < STRING_LENGTH){
            break;
        }
    }
}

void transferLine(int lineNumber){
    strncpy(outputText[yl], text[lineNumber-1], STRING_LENGTH+1);
    ++yl;
}

void transferLines(int start, int finish){
    for(int line = start; line <= finish; ++line){
        transferLine(line);
    }
}

void showText(char text[NUMBER_OF_STRINGS][STRING_LENGTH+1]){
    int lineNumber = 0;
    while (lineNumber != 20) {
        printf("%i)\t%s\n", lineNumber+1, text[lineNumber]);
        ++lineNumber;
    }
}

int getInt(){
    int r = 0;
    char *p = strtok('\0', " ");
    if (p && p[0] != EOF) {
        r = atoi(p);
        
        if (r != 0) {
            return r;
        }
    }
    
    exit(EXIT_FAILURE);
}

char* getString(){
    return strtok('\0', "");
}

void setText(char *str){
    int length = (int)strlen(str);
    int linesCount = (int)length / STRING_LENGTH;
    if (length % STRING_LENGTH) {
        linesCount += 1;
    }
    
    for (int i = 0; i < NUMBER_OF_STRINGS; ++i) {
        memset(text[i], 0, sizeof(text[i]));
    }
    
    for (int i = 0; i < linesCount; ++i) {
        strncpy(text[i], str + i * STRING_LENGTH , STRING_LENGTH);
    }
}

int main(int argc, const char * argv[]) {
    char *str = "A best practice is a method or technique that has consistently shown results superior to those achieved with other means, and that is used as a benchmark. In addition, a \"best\" practice can evolve to become better as improvements are discovered. Best practice is considered by some as a business buzzword, used to describe the process of developing and following a standard way of doing things that multiple organizations can use.";
    setText(str);
    showText(text);
    
    showText(outputText);
    exit(EXIT_SUCCESS);
    while (1) {
        char str[128];
        fgets(str, 126, stdin);
        
        char *p;
        
        p = strtok(str, " ");
        m = getInt();

        if (strncmp(p, "I", 1) == 0) {
            p = getString();
            insert(m, p);
            
        } else if (strncmp(p, "D", 1) == 0){
            n = getInt();
            delete(m, n);
            
        } else if (strncmp(p, "R", 1) == 0){
            n = getInt();
            p = getString();
            
            replace(m, n, p);

        } else if (strncmp(p, "E", 1) == 0){
            exit(EXIT_SUCCESS);
        }
    }
    
    return 0;
}
