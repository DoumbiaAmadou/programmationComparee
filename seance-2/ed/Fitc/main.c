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
#include <fcntl.h>

#define NUMBER_OF_STRINGS   100
#define STRING_LENGTH       80
#define EXIT                69
#define TRUE                1
#define INSERT_CMD          'I'
#define DELETE_CMD          'D'
#define REPLACE_CMD         'R'
#define EXIT_CMD            'E'

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

void interpretCommand(char *command){
    char *p = strtok(command, " ");
    char cmd = p[0];
    
    switch (cmd) {
        case INSERT_CMD:
            m = getInt();
            p = getString();
            
            putLinesToOutput(xl, m);
            putCurrentLineToOutput();
            putLineToOutput(p);
            
            break;
        
        case DELETE_CMD:
            m = getInt();
            n = getInt();
            
            putLinesToOutput(xl, m);
            xl = n+1;
            
            break;
            
        case REPLACE_CMD:
            m = getInt();
            n = getInt();
            p = getString();
            
            putLinesToOutput(xl, m);
            putLineToOutput(p);
            xl = n+1;
            
            break;
            
        case EXIT_CMD:
            
            putLinesToOutput(xl, m);
            putRestOfInputToOutput();
            showText(outputText);
            exit(EXIT_SUCCESS);
            
            break;
            
        default:
            perror("Unknown command");
            exit(EXIT_FAILURE);
            break;
    }
}



int main(int argc, const char * argv[]) {
    char *s = "A best practice is a method or technique that has consistently shown results superior to those achieved with other means, and that is used as a benchmark. In addition, a \"best\" practice can evolve to become better as improvements are discovered. Best practice is considered by some as a business buzzword, used to describe the process of developing and following a standard way of doing things that multiple organizations can use.";
    splitByStrings(s);
    showText(inputText);

    while (TRUE) {
        printf("Input command: \n");
        char cmd[128];
        fgets(cmd, 126, stdin);
        interpretCommand(cmd);
        showText(outputText);
    }
    
    return 0;
}
