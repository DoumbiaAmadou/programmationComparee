#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define BUFSIZE 100

int ln = 1, in = -1, out = -1;

typedef struct command {
  char c;
  int arg1;
  int arg2;
  int len_t;
  char text[BUFSIZE];
}command;

void read_instruction(command *c);
void add_line(int position, char *s, int len_s);
void replace_lines(int begin, int end, char *s, int len_s, char mode);
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
  do {
    read_instruction(&cmd);
    interpret_instruction(&cmd);
  }while(!exit_ed(&cmd));
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
    if(*tmp != ' ') arg = arg * 10 + (*tmp - '0');
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
    while(*tmp != '\0') {
      cmd->text[i++] = *tmp;
      tmp++;
    }
    cmd->len_t = i;
  }
}

void add_line(int position, char *s, int len_s) {
  char buf;
  position++;
  while(read(in, &buf,1) > 0) {
    if(buf == '\n') ln++;
    if(ln < position || ln > position) {
      write(out,&buf,1);
    } else {
      write(out,"\n",1);
      write(out,s,len_s);
      ln++;
    }
  }
}


void replace_lines(int begin, int end, char *s, int len_s, char mode) {
  char buf;
  int writer_line = end - begin;
  while(read(in,&buf,1) > 0) {
    if(buf == '\n') ln++;
    if(ln < begin || ln > end) {
      if((write(out,&buf,1)) < 0) {
      	perror("Error write");
      	exit(EXIT_FAILURE);
      }
    } else {
      if(mode == 'R') {
	if(writer_line-- >= 0) {
	  if(write(out,"\n",1) < 0) {
	    perror("Error write");
	    exit(EXIT_FAILURE);
	  }
	  if((write(out,s,len_s-1)) < 0) {
	    perror("Error write");
	    exit(EXIT_FAILURE);
	  }
	}
      }
    }
  }
}
   
void interpret_instruction(command *cmd) {
  switch(cmd->c) {
  case 'I':
    if(cmd->arg1 < ln) {
      printf("arg 1 = %d && line number = %d\n", cmd->arg1, ln);
      printf("Bad number of line.");
      exit(EXIT_FAILURE);
    }
    add_line(cmd->arg1,cmd->text,cmd->len_t);
    break;
  case 'R':
    if(cmd->arg1 < ln) {
      printf("Bad number of line.");
      exit(EXIT_FAILURE);
    }
    replace_lines(cmd->arg1,cmd->arg2, cmd->text,cmd->len_t, cmd->c);
    break;   
  case 'D':
    replace_lines(cmd->arg1, cmd->arg2, NULL,-1, 'D');
    break;
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



