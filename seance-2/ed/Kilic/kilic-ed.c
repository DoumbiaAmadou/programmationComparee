#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BUFSIZE 80

void read_instruction(char buf[BUFSIZE]);

void interpret_instruction(char buf[BUFSIZE]);

int main(int argc, char **argv) {
  char buf[BUFSIZE];
  do {
    read_instruction(buf);
    interpret_instruction(buf);
  }while(buf[0] != 'E');
  return 0;
}

void read_instruction(char buf[BUFSIZE]) {
}

void interpret_instruction(char buf[BUFSIZE]) {
}


