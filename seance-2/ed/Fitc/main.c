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
#define TRUE                1

char inputText[NUMBER_OF_STRINGS][STRING_LENGTH+1];
char outputText[NUMBER_OF_STRINGS][STRING_LENGTH+1];
int m, n, xl = 0, yl = 0;
char ln[STRING_LENGTH+1];

void splitByStrings(char *p){
    for (int i = 0; i < NUMBER_OF_STRINGS; ++i) {
        char *s = strncpy(inputText[i], &p[i*STRING_LENGTH], STRING_LENGTH);
        if (strlen(s) < STRING_LENGTH){
            break;
        }
    }
}

void putLineToOutput(char *line){
    strncpy(outputText[yl], line, STRING_LENGTH+1);
    ++yl;
}

void putLineWithNumberToOutput(int lineNumber){
    putLineToOutput(inputText[lineNumber]);
}

void putCurrentLineToOutput(){
    putLineWithNumberToOutput(xl);
    xl++;
}

void putRestOfInputToOutput(){
    while (inputText[xl][0] != '\0') {
        putCurrentLineToOutput();
    }
}

void putLinesToOutput(int start, int finish){
    for(int line = start; line < finish; ++line){
        putLineWithNumberToOutput(line);
    }
    xl = finish;
}

void showText(char text[NUMBER_OF_STRINGS][STRING_LENGTH+1]){
    int lineNumber = 0;
    while (text[lineNumber][0] != '\0') {
        printf("%i)\t%s\n", lineNumber, text[lineNumber]);
        ++lineNumber;
    }
}

int getInt(){
    int r = 0;
    char *p = strtok('\0', " ");
    char *end;
    if (p && p[0] != EOF) {
        r = (int)strtol(p, &end, 10);
        
        if (p != end) {
            return r;
        }
    }
    
    return -1;
}

char* getString(){
    return strtok('\0', "");
}

int main(int argc, const char * argv[]) {
    char *s = "A best practice is a method or technique that has consistently shown results superior to those achieved with other means, and that is used as a benchmark. In addition, a \"best\" practice can evolve to become better as improvements are discovered. Best practice is considered by some as a business buzzword, used to describe the process of developing and following a standard way of doing things that multiple organizations can use.";
    splitByStrings(s);
    showText(inputText);

    while (TRUE) {
        printf("Input command: \n");
        char str[128];
        fgets(str, 126, stdin);
        char *p;
        
        p = strtok(str, " ");
        m = getInt();
        if (m == -1) {
            m = xl;
        }
        
        putLinesToOutput(xl, m);
        if (strncmp(p, "I", 1) == 0) {
            p = getString();
            putCurrentLineToOutput();
            putLineToOutput(p);
            
        } else if (strncmp(p, "D", 1) == 0){
            n = getInt();
            if (n == -1) {
                n = m;
            }
            xl = n+1;
            
        } else if (strncmp(p, "R", 1) == 0){
            n = getInt();
            if (n == -1) {
                n = m;
            }
            p = getString();
            putLineToOutput(p);
            xl = n+1;

        } else if (strncmp(p, "E", 1) == 0){
            putRestOfInputToOutput();
            showText(outputText);
            exit(EXIT_SUCCESS);
        }
        
        showText(outputText);
    }
    
    return 0;
}
