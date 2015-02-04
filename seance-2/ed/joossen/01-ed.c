#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/stat.h>
#include <sys/types.h>

int entry, outfile, ln;


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
