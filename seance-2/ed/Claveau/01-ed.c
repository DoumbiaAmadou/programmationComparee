#include <stdio.h>
#include <unistd.h>

typedef struct cmd {
    char instruction;
    int arg1;
    int arg2;
    char* content;
    int contentSize;
} command;

void execute() {
    int position;
    command instruct;
    int end = 1;
    do {
        instruct = readInstruction();
        end = execute_cmd(instruct)
    } while (end > 0);
}

command readInstruction() {
    
}

int execute_cmd(command c) {
    return 0;
}

int main (int argc, char** argv) {
    if(argc < 3)
        exit(1);
    int fin = open(argv[1],O_RDONLY);
    int fout = open(argv[2],O_WRONLY);
    execute(fin, fout);
    close(fin);
    close(fout);
}
