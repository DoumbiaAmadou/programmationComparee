#include <unistd.h>

char * read_instr(){
}

int not_end(char c){
	return c != 'E'
}

char * load_file(char * fic){
}


int main(int argc, char ** argv){
	char * text = load_file(argv[1]);
	char * instr = read_instr();
	while(not_end(instr[0])){
		interp_instr(instr);
		instr = read_instr();
	}
}
