#include <unistd.h>
#include <stdio.h>

#define SIZE 80

int read_instruction(char *buf);

int interpret_instruction(char *buf);

int main(int argc, char **argv) {

    if(argc < 3) {
      exit(1);
    }

    int fd_in  = open(argv[1], O_RDONLY);
    int fd_out = open(argv[2], O_WRONLY|O_CREAT, 0664);
    char buf[SIZE] = {'\0'};

    do {

        read_instruction(buf);
        interpret_instruction(buf);

    }while(buf[0] != 'E');

    close(fd_in);
    close(fd_out);

}
