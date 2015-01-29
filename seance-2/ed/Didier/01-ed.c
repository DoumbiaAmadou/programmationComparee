#include <stdio.h>

typedef command;
command read_cmd();
int isExit(command c);

typedef text;
text load(char * f);
void print(text t);
text apply(command c, text t);

text ed(const text in){
  command cmd = read_cmd();
  while(!isExit(cmd)){
    in = apply(cmd,in);
    cmd = read_cmd();
  }
  return in;
}

int main(int argc, char** argv){
  if(argc < 2) exit(1);
  text t = load(argv[1]);
  text res = ed(t);
  print(res);
}
