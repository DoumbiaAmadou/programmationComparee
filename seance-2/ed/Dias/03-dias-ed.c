#include <unistd.h>
#include <stdio.h>

#define SIZE 80

/*
    Raffinement 3 : Definition de get_parameters
*/

struct parameters_t {
    char cmd;
    int m;
    int n;
    char text[SIZE];
};

int read_instruction(char *buf);

int interpret_instruction(char *buf, int fd_in, int cur_line, int fd_out);

int get_parameters(char *buf, struct *parameters_t);

void copy_lines(int from, int to, int fd_in, int fd_out);

void write_line(int fd, char *text);

void jump_to_line(int fd, int to_line);

int main(int argc, char **argv) {

    if(argc < 3) {
      exit(EXIT_FAILURE);  
    }

    int fd_in  = open(argv[1], O_RDONLY);
    int fd_out = open(argv[2], O_WRONLY|O_CREAT, 0664);
    int continue_edition = 1;
    int error = 0;
    int result = 0;

    int cur_line = 1;
    char buf[SIZE] = {'\0'};
   
    do {

        if(result = read_instruction(buf)) {
            result = interpret_instruction(buf, fd_in, cur_line, fd_out);
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

int read_instruction(char *buf) {

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

int interpret_instruction(char *buf, int fd_in, int cur_line, int fd_out) {

    if(buf[0] == 'E') {
        return 0;
    }
    
    struct parameters_t parameters;

    if(get_parameters(buf, &parameters) > 0) {

        switch(parameters.cmd) {
            case 'I':
            copy_lines(cur_line, parameters.m, fd_in, fd_out);
            write_text(fd_out, parameters.text);
            cur_line = parameters.m + 1;
            break;

            case 'R':
            write_text(fd_out, parameters.text);
            case 'D':
            jump_to_line(fd_in, parameters.n);
            cur_line = parameters.n;
            break;    
        }

        return cur_line;
    }

    return -1;
}

int get_parameters(char *buf, struct parameters_t *params) {
    
    char m:
    char n;  

    int i,j;

    for(i = 0; i < 2 && buf[i] != '\0'; i++);

    if(i == 2){
        m = buf[i];
        if(m >= '0' && m <= '9')
            params->m = m - '0';
        else
            return -1;
    }
    else {
        return -1;
    }

    switch(buf[0]) {
        case 'R':
        for(i=2; i < 4 && buf[i] != '\0'; i++);
        if(i == 4) {
            n = buf[i];
            if(n >= '0' && n <= '9')
                params->n = n - '0';
            else
                return -1;
        }

        case 'I':
        if(buf[++i] != ',')
            return -1;
        j = 0;
        while(buf[i] != '\0'){
            param[j] = buf[i];
            i++;
            j++;
        }
        param[j] = '\0';
        break;

        case 'D':
        for(i=2; i < 4 && buf[i] != '\0'; i++);
        if(i == 4) {
            n = buf[i];
            if(n >= '0' && n <= '9')
                params->n = n - '0';
            else
                return -1;
        }
    }

    return 1;
}
