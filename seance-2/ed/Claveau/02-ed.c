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
        instruct = readInstruction(position);
        if (!instruct.code == 'X')
            end = execute_cmd(instruct, position)
    } while (end > 0);
}


int execute_cmd(command c, int position) {
    if (c.code=='E')
        return 0;
    while (c.arg1>position) {
        //copy until arg1
        
    }
    if (c.code!='I') {
        // delete
        return 1;
    }
    else {
        // copy until arg2
    }
    if (c.code!='D') {
        // add
        return 1;
    }
}

command readInstruction(int currentLine) {
    command c;
    char* line;
    gets(line);
    c.code = line[0];
    if (c.code != 'E' && c.code != 'R' && c.code != 'I' && c.code != 'D') {
        c.code = 'X';
        printf("syntax error\n");
        return c;
    }
    if (c.code == 'E')
        return c;
    int i = 2
    while (line[i]==' ') {
        i++;
    }
    int pos=0;
    while (isdigit(line[i])) {
        pos = pos*10 + line[i];
    }
    if (pos<currentLine) {
        printf("line error\n");
        c.code = 'X';
        return c;
    }
    c.arg1 = pos;
    i++;
    if (!c.code=="I") {
        while (line[i]==' ') {
            i++;
        }
        pos=0;
        while (isdigit(line[i])) {
            pos = pos*10 + line[i];
        }
        if (pos<currentLine) {
            printf("line error\n");
            c.code = 'X';
            return c;
        }
        c.arg2 = pos;
        if (c.code == 'D')
            return c;
    }
    i++;
    c.contentSize=0;
    while (1) { // while line not end and length < size buffer 
        c.content[c.contentSize] = line[i];
        c.contentSize ++;
    }
    return c;
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
