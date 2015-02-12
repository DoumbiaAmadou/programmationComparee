#include "01-ed.h"

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BUFFER_SIZE 256

int main (int argc, char ** argv) {
  int fdIn, fdOut;
  // checking that files' names has been provided 
  if (argc <= 2) {
    Exception("Need file names as input and output");
  }
  // opening files
  if ( (fdIn = openFile(argv[1], O_RDONLY)) == -1) {
    perror("Could not open file");
  }
  if ( (fdOut = openFilePerm(argv[2], O_CREAT|O_WRONLY, S_IRWXU)) == -1) {
    perror("Could not open file");
  }
  // looping
  ed(fdIn, fdOut);
  // closing files
  if (closeFile(fdIn) == -1) {
    perror("Could not close file");
  }
  if (closeFile(fdOut) == -1) {
    perror("Could not close file");
  }
  return EXIT_SUCCESS;
}


int ed (int fdIn, int fdOut) {
  // loop
  int ln = 0;
  command_t cmd;
  do {
    fprintf(stdout, "Line %d. Command? \n", ln);
    cmd = readCommand();
    if (!commandIsValid(cmd, ln)) {
      Exception("Invalid command");
    } else {
      ln = interpret(cmd, ln, fdIn, fdOut);
    }
  } while (cmd.instruction != END);
  // copy the rest of the file
  return 1;
}


int openFile (char * path, int flag) {
  return open(path, flag);
}

int openFilePerm (char * path, int flag, mode_t perm) {
  return open(path, flag, perm);
}


int closeFile (int fd) {
  return close(fd);
}


command_t readCommand () {
  command_t command = {-1, -1, -1, " "};
  return command;
}


int interpret (command_t cmd, int line, int fdIn, int fdOut) {
  return 1;
}


int commandIsValid (command_t cmd, int line) {
  return 1;
}

