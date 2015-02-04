#include <unistd.h>

/**
 * Description de la boucle interactive
 * et des flux entrée/sortie
**/

void ed(const int fdin, const int fdout){
  command cmd = read_cmd();
  while(!isExit(cmd)){
    in = apply(cmd,fdin,fdout);
    cmd = read_cmd();
  }
}

int main(int argc, char** argv){
  if(argc < 3) exit(1);
  int in = open(argv[1],O_RDONLY);
  int out = open(argv[2],O_WRONLY);
  ed(in,out);
  close(in);
  close(out);
}
