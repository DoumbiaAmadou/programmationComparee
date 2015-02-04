#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Definition des commandes 
 * et de leur interpretation
**/

typedef struct cmd {
  char code;
  int arg1;
  int arg2;
  char * text;
  int tlen;
} command;

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

void ed(const int fdin, const int fdout){
  command cmd = read_cmd();
  while(!isExit(cmd)){
    apply(cmd,fdin,fdout);
    cmd = read_cmd();
  }
}

int main(int argc, char** argv){
  if(argc < 3) exit(1);
  int in = open(argv[1],O_RDONLY);
  int out = open(argv[2],O_WRONLY);
  ed(in,out);
  close(in);
  close(out);
}
