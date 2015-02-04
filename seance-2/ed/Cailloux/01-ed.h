#ifndef ED_HPP_0_1
#define ED_HPP_0_1

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

int openFile (char * path, int flag);
int openFilePerm (char * path, int flag, mode_t perm);

int closeFile (int fd);

command_t readCommand ();

int interpret (command_t cmd, int line, int fdIn, int fdOut);

int commandIsValid (command_t cmd, int line);

int ed (int fdIn, int fdOut);

#endif
