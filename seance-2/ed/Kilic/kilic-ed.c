#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define BUFSIZE 100

int ln = 0, in = -1, out = -1;

typedef struct command {
  char c;
  int arg1;
  int arg2;
  int len_t;
  char text[BUFSIZE];
}command;

void read_instruction(command *c);
void copy_n_lines(int m, char *s, int len_s);
void interpret_instruction(command *c);
int exit_ed(command *c);
void print_command(command *c);

int main(int argc, char **argv) {
  command cmd = {' ', -1, -1, 0};
  in = open(argv[1],O_RDONLY);
  if(in < 0) {
    perror("Error open");
    exit(EXIT_FAILURE);
  }
  out = open(argv[2], O_RDWR | O_TRUNC | O_CREAT, 0644);
  if(out < 0) {
    perror("Error open");
    exit(EXIT_FAILURE);
  }
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
    cmd->len_t = i;
  }
}

void copy_n_lines(int m, char *s, int len_s) {
  char buf;
  int r;
  while(ln < m) {
    if((r = read(in,&buf,1)) < 0) {
      perror("Error read");
      exit(EXIT_FAILURE);
    }
    if(buf == '\n') ln++;
    if((write(out,&buf,1) < 0)) {
      perror("Error read");
      exit(EXIT_FAILURE);
    }
  }
  if((write(out,s,len_s) < 0)) {
    perror("Error read");
    exit(EXIT_FAILURE);
  }
}

    

void interpret_instruction(command *cmd) {
  switch(cmd->c) {
  case 'I':
    if(cmd->arg1 < ln) {
      printf("Bad number of line.");
      exit(EXIT_FAILURE);
    }
    copy_n_lines(cmd->arg1,cmd->text,cmd->len_t);
  }
}

int exit_ed(command *cmd) {
  return (cmd->c == 'E');
}

void print_command(command *cmd) {
  printf("Command = %c\n", cmd->c);
  printf("Argument 1 = %d\n", cmd->arg1);
  printf("Argument 2 = %d\n", cmd->arg2);
  printf("Text = %s\n", cmd->text);
  printf("Size of text = %d\n", cmd->len_t);
}



