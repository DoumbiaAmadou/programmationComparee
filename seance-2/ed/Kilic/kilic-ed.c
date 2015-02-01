#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BUFSIZE 80

struct cmd {
  char c;
  int arg1;
  int arg2;
  char text[BUFSIZE];
}cmd;

void read_instruction(struct cmd *c);

void interpret_instruction(struct cmd *c);

void print_command(struct cmd *c);

int main(int argc, char **argv) {
  struct cmd c;
  read_instruction(&c);
  print_command(&c);
  interpret_instruction(&c);
  return 0;
}

void read_instruction(struct cmd *c) {
  char line[BUFSIZE], *tmp;
  ssize_t r;
  int arg,i;
  r = read(STDIN_FILENO,line,BUFSIZE);
  if(r > BUFSIZE) {
    printf("Error : the MAX length of your command must be 80. Goodbye !");
    exit(EXIT_FAILURE);
  }
  line[r] = '\0';
  tmp = line;
  cmd.c = line[0];
  tmp++; tmp++;
  arg = 0;
  if(line[0] == 'E') return;
  while(*tmp != ',') {
    arg = arg * 10 + (*tmp - '0');
    tmp++;
  }
  cmd.arg1 = arg;
  tmp++;
  if(line[0] == 'R' || line[0] == 'D') {
    arg = 0;
    while(*tmp != ',' && *tmp != '\n') {
      arg = arg * 10 + (*tmp - '0');
      tmp++;
    }
    if(*tmp == ',') tmp++;
    cmd.arg2 = arg;
  }
  if(line[0] == 'I' || line[0] == 'R') {
    i = 0;  
    while(*tmp != '\n') {
      cmd.text[i++] = *tmp;
      tmp++;
    }
  }
}

void interpret_instruction(struct cmd *c) {
}

void print_command(struct cmd *c) {
  printf("Command = %c\n", cmd.c);
  printf("Argument 1 = %d\n", cmd.arg1);
  printf("Argument 2 = %d\n", cmd.arg2);
  printf("Text = %s\n", cmd.text);
}


