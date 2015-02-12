#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

#define INSTR_LENGTH 85

int ed(int in, int out);
int readInstruction(char *instruction);
int interpretInstruction(char *instruction, int in, int ln, int out);

int main(int argc, char* argv[]) {
  int in, out;

  if(argc < 3) {
    return EXIT_FAILURE;
  }

  in = open(argv[1], O_RDONLY);
  out = open(argv[2], O_WRONLY | O_CREAT, 0744);

  ed(in, out);
  
  close(in);
  close(out);

  return EXIT_SUCCESS;
}

int ed(int in, int out) {  
  char instruction[INSTR_LENGTH];
  int ln = 0, state;

  do {
    int wc = write(STDOUT_FILENO, "Veuillez entrez une instruction :", 33);
    int rc = read(STDIN_FILENO, instruction, INSTR_LENGTH);
    
    if(rc < 0) {
      return EXIT_FAILURE;
    }

    if(wc < 0) {
      return EXIT_FAILURE;
    }

    state = readInstruction(instruction);

    if(!state) {
      interpretInstruction(instruction, in, ln, out);
    } else {
      return EXIT_FAILURE;
    }
  
  } while(instruction[0] != 'E');

  return EXIT_SUCCESS;
}

int readInstruction(char *instruction) {
  int i;
  
  if(instruction[0] != 'E' && instruction[0] != 'I' && instruction[0] != 'D' &&
     instruction[0] != 'R') {
    return EXIT_FAILURE;
  }  

  if(instruction[0]== 'E') {
    return EXIT_SUCCESS;
  }

  if((instruction[0] == 'R' || instruction[0] == 'D' || instruction[0] == 'I') 
     && instruction[1] != ',' || (instruction[2] < 48 || instruction[2] > 57)) {
    return EXIT_FAILURE;
  } 

  if((instruction[0] == 'R' || instruction[0] == 'D') && instruction[1] == ',' 
     && (instruction[2] >= 48 && instruction[2] <= 57)) {
    for(i = 3; instruction[i] >= 48 && instruction[i] <= 57; i++) {
    }
    if(instruction[i] != ',' ||
       (instruction[i + 1] < 48 || instruction[i + 1] > 57)) {
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}

int interpretInstruction(char *instruction, int in, int ln, int out) {
  return EXIT_SUCCESS;
}
