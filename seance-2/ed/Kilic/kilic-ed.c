#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BUFSIZE 80

typedef struct command {
  char c;
  int arg1;
  int arg2;
  char text[BUFSIZE];
}command;

void read_instruction(command *c);
void interpret_instruction(command *c);
int exit_ed(command *c);
void print_command(command *c);

int main(int argc, char **argv) {
  command cmd = {' ', -1, -1};
  read_instruction(&cmd);
  print_command(&cmd);
  interpret_instruction(&cmd);
  return 0;
}

void read_instruction(command *cmd) {
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
  cmd->c = line[0];
  tmp++; tmp++;
  arg = 0;
  if(line[0] == 'E') return;
  while(*tmp != ',') {
    arg = arg * 10 + (*tmp - '0');
    tmp++;
  }
  cmd->arg1 = arg;
  tmp++;
  if(line[0] == 'R' || line[0] == 'D') {
    arg = 0;
    while(*tmp != ',' && *tmp != '\n') {
      arg = arg * 10 + (*tmp - '0');
      tmp++;
    }
    if(*tmp == ',') tmp++;
    cmd->arg2 = arg;
  }
  if(line[0] == 'I' || line[0] == 'R') {
    i = 0;  
    while(*tmp != '\n') {
      cmd->text[i++] = *tmp;
      tmp++;
    }
  }
}

void interpret_instruction(command *c) { }

int exit_ed(command *cmd) {
  return (cmd->c == 'E');
}

void print_command(command *cmd) {
  printf("Command = %c\n", cmd->c);
  printf("Argument 1 = %d\n", cmd->arg1);
  printf("Argument 2 = %d\n", cmd->arg2);
  printf("Text = %s\n", cmd->text);
}


