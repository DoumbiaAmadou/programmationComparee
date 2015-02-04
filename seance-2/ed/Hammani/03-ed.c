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



int main(int argc, char ** argv){
	char * text = load_file();
	char * instr = read_instr();
	while(not_end(instr[0])){
		interp_instr(instr);
		instr = read_instr();
	}
}
