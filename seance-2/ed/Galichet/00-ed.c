#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

typedef struct cmd{

} cmd_t;

int isExist(cmd_t cmd);

cmd_t catch_cmd();

int isInvalid(cmd_t cmd);

int editor(const int fd_in,const int fd_out){
    // catch command
    cmd_t cmd = catch_cmd();
    while(isInvalid(cmd) || isExist(cmd)){
        /* execute commande (read fdin) in -> new_in
         * offset is increment and never decrement
         * if offset in end-file cmd finish
         * catch new cmd
         */
        //out = exec_cmd(cmd, fd_in)
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
