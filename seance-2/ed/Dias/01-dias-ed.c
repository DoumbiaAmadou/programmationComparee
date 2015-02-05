#include <unistd.h>
#include <stdio.h>

#define SIZE 80

/*
    Rafinement 1 : Definition de read_instruction
    read_instruction retournera 1 si la lecture est valide
    -1 est renvoyé lors d'une erreur de lecture ou d'une lecture invalide

    Justification : 
    On peut ainsi effecter une gestion d'erreur après avoir lu une commande
    pour decider si on l'interprète ou non
*/

int read_instruction(char *buf);

int interpret_instruction(char *buf);

int main(int argc, char **argv) {

    if(argc < 3) {
      exit(EXIT_FAILURE);  
    }

    int fd_in  = open(argv[1], O_RDONLY);
    int fd_out = open(argv[2], O_WRONLY|O_CREAT, 0664);
    int continue_edition = 1;
    int error = 0;
    int result = 0;
    char buf[SIZE] = {'\0'};
   
    do {

        if(result = read_instruction(buf)) {
            interpret_instruction(buf);
        }
        else {
            error = 1;
            continue_edition = 0;    
        }

    }while(continue_edition);

    close(fd_in);
    close(fd_out); 

    if(error)
        exit(EXIT_FAILURE);
    else
        exit(EXIT_SUCCESS);

}

void read_instruction(char *buf) {

    int read_bytes = read(STDIN_FILENO, buf, SIZE);

    if(read_bytes == -1) {
        return -1;
    }    
    else if (read_bytes == SIZE) {
        if(buf[SIZE-1] != '\n') 
            return -1;
    }

    return 1;
}
