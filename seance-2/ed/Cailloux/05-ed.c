#include "05-ed.h"

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
  copyRest (fdIn, fdOut);
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
  shift (cmd, line, fdIn, fdOut);
  switch (cmd.instruction) {
  case INSERT:
    break;
  case DELETE:
    break;
  case REPLACE:
    break;
  case END:
    break;
  default:
    break;
  }
  return cmd.line_snd;
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

int copy (int n, int fdIn, int fdOut) {
  char buffer[BUFFER_SIZE], * buffer_head;
  char * pre, * post, * line_break;
  char * newline = "\n";
  int r = -1, line_len = 0;
  int count = 1;
  while ( count <= n && (r=read(fdIn, buffer, BUFFER_SIZE)) != 0) {
    // error handling
    if (r == -1) {
      perror("Problem while reading");
      exit(EXIT_FAILURE);
    }
    // checking for \n
    buffer_head = &buffer[0];
    line_break = strchr(buffer, '\n');
    if (line_break == NULL) {
      // no '\n', so copying everything
      write(fdOut, buffer, r);
    } else {
      // there is at least 1 line break
      do {
	// writing exactly the number of char of the line
	line_break = strchr(buffer_head, '\n');
	if (line_break) *line_break = '\0';  // temporarily terminate the current line
	printf("curLine=[%s]\n", buffer_head);
	if (line_break) *line_break = '\n';  // then restore newline-char, just to be tidy    
	if (write(fdOut, buffer_head, BUFFER_SIZE) <= 0) {
	  perror("Could not write");
	  exit(EXIT_FAILURE);
	}
	buffer_head = line_break ? (line_break+1) : NULL;
	count++;
      } while (count <= n && line_break != NULL);
    }
  }
  return 1;
}

int shift (command_t cmd, int line, int fdIn, int fdOut) {
  int diff = cmd.line - line;
  if (diff > 0) {
    copy (diff, fdIn, fdOut);
  }
  return diff;
}

int copyRest (int fdIn, int fdOut) {
  char buffer[BUFFER_SIZE];
  int r = -1;
  while ((r=read(fdIn, buffer, BUFFER_SIZE)) != 0) {
    write(fdOut, buffer, r);
  }
  return 1;
}
