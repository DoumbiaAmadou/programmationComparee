#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define SIZE_MAX 128


typedef enum {
    INSERT, 
    REPLACE, 
    DELETE, 
    EXIT,
    INVALID
} cmdcode_t;

typedef struct cmd{
    cmdcode_t code;
    int m;
    int n;
    char *text;
} cmd_t;

int isExist(cmd_t cmd){
    if(cmd.code == EXIT || cmd.code == INVALID) return 0;
    return 1;
}

cmd_t catch_cmd(){
    cmd_t cmd;
    char buff[SIZE_MAX];
    char text[SIZE_MAX];
    int m,n;
    ssize_t cmd_read;
    
    cmd_read = read(STDIN_FILENO, buff, SIZE_MAX);

    // cmd :I,m,TEXT...
    if (sscanf(buff, "I,%d,%s", &m ,text) == 1) {
        cmd.code = INSERT;
        cmd.m = m;
        cmd.text = text;
        return cmd; 
    }
    
    // cmd :R,m,n,TEXT...
    if (sscanf(buff, "R,%d,%n,%s", &m, &n, text) == 1) {
        cmd.code = REPLACE;
        cmd.m = m;
        cmd.n = n;
        cmd.text = text;
        return cmd; 
    }

    // cmd :D,m,n
    if (sscanf(buff, "D,%d,%d", &m ,&n) == 1) {
        cmd.code = DELETE;
        cmd.m = m;
        cmd.n = n;
        cmd.text = text;
        return cmd; 
    }
 
    // cmd :E
    if (buff[0] == 'E' && cmd_read == 1){
        cmd.code = EXIT;
        return cmd;
    }

    cmd.code = INSERT;
    return cmd;
}

int exec_cmd(cmd_t cmd, const int in){
    return 0;
}

int editor(const int fd_in,const int fd_out){
    int out;
    // catch command
    cmd_t cmd = catch_cmd();
    while(isExist(cmd)){
        /* execute commande (read fdin) in -> new_in
         * offset is increment and never decrement
         * if offset in end-file cmd finish
         * catch new cmd
         */
        out = exec_cmd(cmd, fd_in);
        cmd_t cmd = catch_cmd();
    }
    // when finish write out to fdout
    return EXIT_SUCCESS;
}

int main(int argc, char** argv){
    int in, out, ret;

    // ed [in] [out] 2 args
    if (argc != 3) 
        return EXIT_FAILURE;

    in = open(argv[1], O_RDONLY);
    out = open(argv[2], O_WRONLY);

    ret = editor(in, out);

    close(in);
    close(out);
    return ret;
}
