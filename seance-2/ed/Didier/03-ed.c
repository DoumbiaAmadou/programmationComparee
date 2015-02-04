#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Definition de l'application d'une commande
**/

int fdin, fdout, ln=0;

typedef struct cmd {
  char code;
  int arg1;
  int arg2;
  char * text;
  int tlen;
} command;

void apply(command c){
  switch(c.code){
  case 'I':
    if(copy(c.arg1))
      write(fdout,c.text,c.tlen);
    else exit(1);
  
  case 'D':
    if(copy(c.arg1) && move(c.arg2));
    else exit(1);
  
  case 'R':
    if(copy(c.arg1) && move(c.arg2))
      write(fdout,c.text,c.tlen);
    else exit(1);
  
  default:
    exit(1);
  }
}

////////////////////////////////////////////////////////////

int isExit(command c){
  return c.code=='E';
}

command read_cmd(){
  command c;
  char buf[128];
  read(STDIN_FILENO,buf,128);
  c.code = buf[0];
  
  int s=0,i;
  for(i = 3; buf[i]>=48 && buf[i]<=57; i++)
    s=s*10+(buf[i]-48);
  c.arg1 = s;
  
  if(c.code=='D' || c.code=='R'){
    s=0;
    for(i += 3; buf[i]>=48 && buf[i]<=57; i++)
      s=s*10+(buf[i]-48);
    c.arg2 = s;
  }
  
  if(c.code=='I' || c.code=='R'){
    int start = i+3;
    char text[80];
    for(i+=3; i<start+80 && buf[i]!='\n'; i++)
      text[i-start] = buf[i];
    c.tlen = i-start+1;
    c.text = text;
  }
  
  return c;
}

////////////////////////////////////////////////////////////


void ed(){
  command cmd = read_cmd();
  while(!isExit(cmd)){
    apply(cmd);
    cmd = read_cmd();
  }
}

int main(int argc, char** argv){
  if(argc < 3) exit(1);
  fdin = open(argv[1],O_RDONLY);
  fdout = open(argv[2],O_WRONLY);
  ed();
  close(fdin);
  close(fdout);
}
