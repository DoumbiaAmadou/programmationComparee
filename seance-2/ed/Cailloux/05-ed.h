#ifndef ED_HPP_0_5
#define ED_HPP_0_5

#include <stdlib.h>

#define CMD_SIZE 80 // max size for user input

// error handling
#define Exception(x) fprintf(stderr, "%s", x); exit(EXIT_FAILURE)
#ifdef DEBUG
#define Debug(x) fprintf(stderr, "DEBUG: %s\n", x)
#else
#define Debug(x) 
#endif

typedef enum {
  INSERT = 1,
  DELETE,
  REPLACE,
  END,
  MAX_CMD
} instruction_t;

typedef struct command {
  instruction_t instruction;
  int line;
  int line_snd;
  char * text;
} command_t;

// opens the file at the given path, with the given flag
// returns : the file descriptor
int openFile (char * path, int flag);
int openFilePerm (char * path, int flag, mode_t perm);

// closes the given descriptor
int closeFile (int fd);

// reads a command from stdin and injects it after treatment
// throws an exception when a command is malformed
command_t readCommand ();

// executes the given command to fdIn, resulting to fdOut,
// and returns the new line index
int interpret (command_t cmd, int line, int fdIn, int fdOut);

// pretty-prints the given command
void print_command (command_t cmd);

// returns 0 if the command is not valid wrt the given line index
// a command is invalid if the instruction is unknown, or if 
// one of its lines is incorrect
int commandIsValid (command_t cmd, int line);

// performs ed with the given file descriptors
int ed (int fdIn, int fdOut);

// checks if there is a need to copy, and performs it if there is
int shift (command_t cmd, int l, int fdIn, int fdOut);

// copies n lines from fdIn to fdOut
// in case of overflow, does not crash
int copy (int n, int fdIn, int fdOut);

// copies the rest of fdIn to fdOut
int copyRest (int fdIn, int fdOut);

#endif
