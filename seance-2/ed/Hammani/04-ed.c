#include <unistd.h>
#include <fcntl.h>

#define BUF_SIZE 256

char * read_instr(){
	printf("-> ");
	char buf[BUF_SIZE];
	int i =  read(STDIN_FILENO,buf,BUF_SIZE);
	if (i==-1)
	  exit(-1);
	return buf;
}

int not_end(char c){
	return c != 'E'
}

char * load_file(char * fic){
  char * c = open(fic,O_RDONLY)
    if (c==-1)
      exit(-1);
    else
      return c;
}


void interp_instr(char * cmd){
  int m,n,mult=0;
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
    for(j;80;j++){
      i++;
      if(cmd[i]=='\0' || cmd[i]=='\n')
	break;
      text[j]= cmd[i];
    }
  }

  switch (cmd[0]){
  case 'I':insert(m,text);
  case 'D':delete(m,n);
  case 'R':replace(m,n,text);
  default: exit(-1);
    
  }

}

int main(int argc, char ** argv){
	char * text = load_file();
	char * instr = read_instr();
	while(not_end(instr[0])){
		interp_instr(instr);
		instr = read_instr();
	}
}
