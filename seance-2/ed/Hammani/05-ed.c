#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

#define BUF_SIZE 256

int fic_in, fic_out,ln=0,text_size;
 
char * read_instr(char * buf){
  char tmp[BUF_SIZE];
  printf("-> ");
  int i =  read(STDIN_FILENO,buf,BUF_SIZE);
  if (i==-1)
    exit(-1);
  buf = tmp;
  return buf;
}

int not_end(char c){
  return c != 'E';
}

void load_file(char * fic, char *fic2){
  fic_in = open(fic,O_RDONLY);
  if (fic_in==-1)
    exit(-1);
  fic_out = open(fic2,O_RDWR|O_CREAT|O_TRUNC); 
}

void copy(int indice){
  char tmp[1];
  while(ln<indice){
    if(tmp[1]=='\n') ln++;
    read(fic_in,tmp,1);
    write(fic_out,tmp,1);
  }

}

void shift(int indice){
  char tmp[1];
  while(ln<indice){
    if(tmp[1]=='\n') ln++;
    read(fic_in,tmp,1);
  }
}

void insert_text(int m,char * text){
  copy(m);
  write(fic_out,text,text_size);
  text_size =0;
}

void delete(int m, int n){
  copy(m);
  shift(n);
}

void replace(int m,int n,char * text){
  copy(m);
  write(fic_out,text,text_size);
  text_size=0;
  shift(n);
}

void interp_instr(char * cmd){
  int m,n;
  char text[80];
  int i;
  for(i=3;cmd[i]>=48 && cmd[i]<=57 ;i++){
    m = m*10 +(cmd[i]-48);
  }

  if(cmd[0] == 'D' || cmd[0] == 'R'){
    for(++i;cmd[i]>=48 && cmd[i]<=57 ;i++){
      n = n*10 +(cmd[i]-48);
    }
  }

  if(cmd[0] == 'I' || cmd[0] == 'R'){
    int j=0;
    for(j;j<80;j++){
      i++;
      if(cmd[i]=='\0' || cmd[i]=='\n')
	break;
      text[j]= cmd[i];
      text_size++;
    }
  }

  switch (cmd[0]){
  case 'I':insert_text(m,text);
  case 'D':delete(m,n);
  case 'R':replace(m,n,text);
  default: exit(-1); 
  }

}

int main(int argc, char ** argv){
  char buf[BUF_SIZE];
  if(argc <3) 
    exit(-1);
  load_file(argv[1],argv[2]);
  char * instr = read_instr(buf);
  while(not_end(instr[0])){
    interp_instr(instr);
    instr = read_instr(buf);
  }
  return 0;
}
