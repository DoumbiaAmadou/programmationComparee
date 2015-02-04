#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/stat.h>
#include <sys/types.h>

#define CMD_SIZE  130
#define LINE_SIZE 80

int entry, outfile, ln, idx_cmd;
char cmd[CMD_SIZE];
char l[LINE_SIZE];

/**
 * Functions declaration by alphabetical order
 **/

/* delete call */
void cmd_delete();
/* exit call */
void cmd_exit();
/* insert call */
void cmd_insert();
/* replace call */
void cmd_replace();
/* instruction interpreter */
void inter_instr();
/* go to the next argument */
void next_arg();
/* when error occurs */
void notif_error(const char*);
/* instruction reader */
void read_instr();
/* change the value of current line content */
void store_line();

/**
 * Function implementation by use order
 **/

void
main(int argc, char** argv)
{
  if (argc < 3)
    notif_error("Missing argument(s)\n");

  mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH;
  if ((entry = open(argv[1], O_RDONLY)) == -1)
    notif_error("Cannot open entry file");
  if ((outfile = open(argv[2], O_WRONLY | O_TRUNC | O_CREAT, mode)) == -1)
    notif_error("Cannot open output file");

  ln = 1;
  store_line();

  while (1) {
    read_instr();
    inter_instr();
  }

  close(entry);
  close(outfile);
}

void
notif_error(const char* msg)
{
  fprintf(stderr, "%s\n", msg);
  _exit(EXIT_FAILURE);
}

void
store_line()
{
  char c;
  off_t new_line_offset;
  size_t cnt_nl;
  int cnt;
  new_line_offset = lseek(entry, 0, SEEK_CUR);
  cnt_nl          = 0;
  while (read(entry, &l[cnt_nl], 1) == 1 &&
         l[cnt_nl] != '\n' && l[cnt_nl] != '\0')
    cnt_nl++;
  for (cnt = cnt_nl; cnt < LINE_SIZE; cnt++) l[cnt] = '\0';
  lseek(entry, -cnt_nl, SEEK_CUR);
}

void
read_instr()
{
  write(STDOUT_FILENO, "> ", 3);
  read(STDIN_FILENO, cmd, CMD_SIZE);
}

void
inter_instr()
{
  char opt;
  idx_cmd = 0;
  if (cmd == NULL || cmd[idx_cmd] == '\0') notif_error("No option");
  opt = cmd[idx_cmd++];
  if (opt == 'E') return cmd_exit();
  next_arg();
  if (opt == 'D') return cmd_delete();
  if (opt == 'I') return cmd_insert();
  if (opt == 'R') return cmd_replace();
  notif_error("Invalid option");
}

void cmd_exit() { _exit(EXIT_SUCCESS); }

void next_arg() { if (cmd[idx_cmd++] != ',') notif_error("',' not found"); }

void
cmd_delete()
{
  int m, n;
  get_int_opt(&m);
  next_arg();
  get_int_opt(&n);
  if (m > n) notif_error("First argument cannot be greater than the second");
  copy_lines(m - 1);
  if (m < ln) notif_error("Invalid line number");
  goto_line(n + 1);
}

void
cmd_insert()
{
  int m;
  size_t text_len;
  get_int_opt(&m);
  if (m < ln) notif_error("Invalid line number");
  next_arg();
  copy_lines(m);
  write_text();
  ln = m + 1;
}

void
cmd_replace()
{
  int m, n;
  get_int_opt(&m);
  next_arg();
  get_int_opt(&n);
  if (m > n) notif_error("First argument cannot be greater than the second");
  next_arg();
  copy_lines(m - 1);
  if (m < ln) return;
  goto_line(n + 1);
  write_text();
}
