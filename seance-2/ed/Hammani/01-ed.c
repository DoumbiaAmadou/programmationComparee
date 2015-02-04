#include <stdio.h>
#include <stdlib.h>



int main(int argc, char ** argv){
	char * text = load_text();
	char * instr = read_instr();
	while(not_end(instr)){
		interp_instr(instr);
		instr = read_instr();
	}
}