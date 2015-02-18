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
    ssize_t text_size;
} cmd_t;

int isExist(cmd_t cmd){
    if(cmd.code == EXIT || cmd.code == INVALID) return 0;
    return 1;
}

int isInvalid(cmd_t cmd){
    if(cmd.code == INVALID) return 0;
    return 1;
}

cmd_t catch_cmd(){
    cmd_t cmd;
    char buff[SIZE_MAX];
    char text[SIZE_MAX];
    int m,n;
    ssize_t cmd_read;
    
    cmd_read = read(STDIN_FILENO, buff, SIZE_MAX);
    /* TODO check finish cmd , if last char is '/n'
     * And create var to determin text size
     */

    // cmd :I,m,TEXT...
    if (sscanf(buff, "I,%d,%s", &m ,text) == 1) {
        cmd.code = INSERT;
        cmd.m = m;
        cmd.text = text;
        cmd.text_size = cmd_read;
        return cmd; 
    }
    
    // cmd :R,m,n,TEXT...
    if (sscanf(buff, "R,%d,%n,%s", &m, &n, text) == 1) {
        cmd.code = REPLACE;
        cmd.m = m;
        cmd.n = n;
        cmd.text = text;
        cmd.text_size = cmd_read;
        return cmd; 
    }

    // cmd :D,m,n
    if (sscanf(buff, "D,%d,%d", &m ,&n) == 1) {
        cmd.code = DELETE;
        cmd.m = m;
        cmd.n = n;
        cmd.text = text;
        cmd.text_size = cmd_read;
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

int insert(const int in, const int m, char *text, ssize_t text_size){
    return -1;
}
int replace(const int in, const int m, const int n, char *text, ssize_t text_size){
    return -1;
}
int delete(const int in, const int m, const int n){
    return -1;
}

int exec_cmd(cmd_t cmd, const int in){
    int out;
    switch(cmd.code){
        case INSERT : out = insert(in, cmd.m, cmd.text, SIZE_MAX); break;
        case REPLACE: out = replace(in, cmd.m, cmd.n, cmd.text, SIZE_MAX);break;
        case DELETE : out = delete(in, cmd.m, cmd.n);break;
        default: out = in ;
    }
    return out;
}

int editor(const int fd_in,const int fd_out){
    int out;
    // catch command ed
    cmd_t cmd = catch_cmd();
    while(isExist(cmd)){
        // execute cmd (read fdin) in -> new_in
        out = exec_cmd(cmd, fd_in);
        if(out == -1){
            printf("Error : failure cmd ");    
            return EXIT_FAILURE;
        }
        cmd = catch_cmd();
    }

    if(isInvalid(cmd)){
        printf("Error : invalid parse cmd");    
        return EXIT_FAILURE;
    }

    // TODO : when finish write out to fdout
    
    return EXIT_SUCCESS;
}

int main(int argc, char** argv){
    int in, out, ret;

    // ed [in] [out] 2 args
    if (argc < 3){
        printf("Error : missing arguments ed [in] [out]\n");    
        return EXIT_FAILURE;
    }

    in = open(argv[1], O_RDWR);
    out = open(argv[2], O_WRONLY);

    ret = editor(in, out);

    close(in);
    close(out);
    return ret;
}
