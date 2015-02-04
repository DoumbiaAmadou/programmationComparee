#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Description de la lecture/ecriture bas niveau
**/

int fdin, fdout, ln=-1;

int copy(const int dest){
  if(dest<ln){
    printf("copy ln:%d dest:%d\n",ln,dest);
    return 0;
  }
  char c[1];
  while(ln<dest){
    if(!read(fdin,c,1))
      return 0;
    write(fdout,c,1);
    if(c[0]=='\n') ln++;
  }
  return 1;
}

int move(const int dest){
  if(dest<ln){
    printf("move ln:%d dest:%d\n",ln,dest);
    return 0;
  }
  if(dest==ln) return 1;
  char c[1];
  while(ln<dest){
    if(!read(fdin,c,1))
      return 0;
    if(c[0]=='\n') ln++;
  }
  return 1;
}

////////////////////////////////////////////////////////////

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
    else exit(EXIT_FAILURE);
    break;
  
  case 'D':
    if(copy(c.arg1-1) && move(c.arg2));
    else exit(EXIT_FAILURE);
    break;
  
  case 'R':
    if(copy(c.arg1-1) && move(c.arg2))
      write(fdout,c.text,c.tlen);
    else
      exit(EXIT_FAILURE);
    break;
    
  default:
    exit(EXIT_FAILURE);
    break;
    
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
  for(i = 2; buf[i]>=48 && buf[i]<=57; i++)
    s=s*10+(buf[i]-48);
  c.arg1 = s;
  
  if(c.code=='D' || c.code=='R'){
    s=0;
    for(++i; buf[i]>=48 && buf[i]<=57; i++)
      s=s*10+(buf[i]-48);
    c.arg2 = s;
  }
  else c.arg2 = -1;
  
  if(c.code=='I' || c.code=='R'){
    int start = i+1;
    char text[80];
    for(++i; i<start+80; i++){
      text[i-start] = buf[i];
      if(buf[i]=='\n') break;
    }
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
  char end[10000];
  int len = read(fdin,end,10000);
  write(fdout,end,len);
}

int main(int argc, char** argv){
  if(argc < 3) exit(1);
  fdin = open(argv[1],O_RDONLY);
  fdout = open(argv[2],O_WRONLY|O_CREAT|O_TRUNC,S_IRWXU);
  ed();
  close(fdin);
  close(fdout);
}
