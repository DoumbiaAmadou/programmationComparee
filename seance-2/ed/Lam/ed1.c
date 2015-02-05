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
  return EXIT_SUCCESS;
}

int interpretInstruction(char *instruction, int in, int ln, int out) {
  return EXIT_SUCCESS;
}
