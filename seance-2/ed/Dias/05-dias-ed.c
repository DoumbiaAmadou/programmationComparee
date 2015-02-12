#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#define SIZE 81

/*
    Raffinement 5 : Definition de jump_to_line
*/

struct parameters_t {
    char cmd;
    int m;
    int n;
    char text[SIZE];
};

int read_instruction(char *buf);

int interpret_instruction(char *buf, int fd_in, int cur_line, int fd_out);

int get_parameters(char *buf, struct parameters_t *params);

int copy_lines(int from, int to, int fd_in, int fd_out);

int jump_to_line(int fd, int from, int to_line);

int write_text(int fd, char *text);

int main(int argc, char **argv) {

    if(argc < 3) {
      exit(EXIT_FAILURE);  
    }

    int fd_in  = open(argv[1], O_RDONLY);
    int fd_out = open(argv[2], O_WRONLY|O_CREAT, 0664);
    int continue_edition = 1;
    int error = 0;
    int result = 0;

    int cur_line = 0;
    char buf[SIZE] = {'\0'};
   
    do {
        result = read_instruction(buf);
        if(result > 0) {
            result = interpret_instruction(buf, fd_in, cur_line, fd_out);
            if(result == -1) {
                continue_edition = 0;
            }
            else if(result == -2) {
                printf("ERROR\n");
                error = 1;
                continue_edition = 0;
            }
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

    int read_bytes = read(STDIN_FILENO, buf, SIZE-1);

    if(read_bytes == -1) {
        return -1;
    }    
    else if (read_bytes == SIZE-1) {
        if(buf[SIZE-2] != '\n') 
            return -1;
    }
    return 1;
}

int interpret_instruction(char *buf, int fd_in, int cur_line, int fd_out) {

    if(buf[0] == 'E') {
        return -1;
    }
    
    struct parameters_t parameters;
    int res = get_parameters(buf, &parameters);
    printf("%d;%c\n",res,parameters.cmd);
    if(res > 0) {

        switch(parameters.cmd) {
            case 'I':
            printf("I\n");
            if(copy_lines(cur_line, parameters.m, fd_in, fd_out)<0)
                return -2;
            write(fd_out, parameters.text, SIZE-4);
            cur_line = parameters.m + 1;
            break;

            case 'R':
            printf("R\n");
            write(fd_out, parameters.text, SIZE-6);
            case 'D':
            printf("D ou R\n");
            if(jump_to_line(fd_in, cur_line, parameters.n) < 0)
                return -2;
            cur_line = parameters.n;
            break;    
        }

        return cur_line;
    }
    printf("ERROR GET\n");
    return -2;
}

int get_parameters(char *buf, struct parameters_t *params) {
   
    char m;
    char n;  

    int i,j;

    params->cmd = buf[0];

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
            params->text[j] = buf[i];
            i++;
            j++;
        }
        params->text[j] = '\0';
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

int copy_lines(int from, int to, int fd_in, int fd_out) {
    if(to <= from)
        return -1;

    char c;

    while(read(fd_in, &c, 1) == 1 && from < to) {
        write(fd_out, &c, 1);
        if(c == '\n')
            from++;
    }

    return 1;
}

int jump_to_line(int fd, int from, int to_line) {

    if(to_line <= from)
        return -1;

    char c;

    while(read(fd, &c, 1) == 1 && from < to_line) {
        if(c == '\n')
            from++;
    }

    return 1;
}
