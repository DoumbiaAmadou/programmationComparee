#include "03-ed.h"

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


void print_command (command_t cmd) {
  printf("command %d : %d -> %d | %s\n", \
	 cmd.instruction, cmd.line, cmd.line_snd, cmd.text);
}

int ed (int fdIn, int fdOut) {
  // loop
  int ln = 0;
  command_t cmd;
  do {
    fprintf(stdout, "Line %d. Command? \n", ln);
    cmd = readCommand();
    if (!commandIsValid(cmd, ln)) {
      print_command(cmd);
      Exception("Invalid command");
    } else {
      //  print_command(cmd);
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
  char cmd_char = '\0';
  command_t command = {-1, -1, -1, " "};
  char cmd_buffer[CMD_SIZE];
  char * buffer_head = NULL;
  char text[CMD_SIZE];
  cmd_buffer[0] = '\0';
  // reading command from stdin
  read(STDIN_FILENO, cmd_buffer, CMD_SIZE);
  cmd_char = cmd_buffer[0];
  buffer_head = (cmd_buffer);
  buffer_head += 2; //now the buffer has passed the instruction letter
  switch (cmd_char) {
  case 'I':
    command.instruction = INSERT;
    if ((sscanf(buffer_head, "%d,%s\n", &command.line, text)) != 2) {
      print_command(command);
      Exception("Problem while parsing Insert instruction");
    }
    command.line_snd = command.line;
    command.text = text;
    break;
  case 'D':
    command.instruction = DELETE;
    if ((sscanf(buffer_head, "%d,%d", &command.line, &command.line_snd)) != 2) {
      print_command(command);
      Exception("Problem while parsing Delete instruction");      
    } 
    command.text = "\0";
    break;
  case 'R':
    command.instruction = REPLACE;
    if ((sscanf(buffer_head, "%d,%d,%s", &command.line, &command.line_snd, text)) != 3) {
      print_command(command);
      Exception("Problem while parsing Replace instruction");      
    }
    command.text = text;
    break;
  case 'E':
    command.instruction = END;
    break;
  default:
    Exception("Unknown instruction");
    break;
  }
  return command;
}


int interpret (command_t cmd, int line, int fdIn, int fdOut) {
  return 1;
}


int commandIsValid (command_t cmd, int line) {
  if (cmd.instruction <= 0 || cmd.instruction >= MAX_CMD){ 
    fprintf(stderr, "Instruction code is unknown");
    return 0;
  } else if (cmd.instruction != END && line > cmd.line) {
    fprintf(stderr, "Instructions cannot go backward");
    return 0;
  } else if ((cmd.instruction == REPLACE || cmd.instruction == DELETE)
	     && (cmd.line_snd < cmd.line)){
    fprintf(stderr, "Instruction arguments are incompatible");
    return 0;
  } else {
    return 1;
  }
}

