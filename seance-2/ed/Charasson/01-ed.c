#include <stdio.h>
#include <stdlib.h>

//INFOS :
//
//pas de malloc !!
//fct rename pour changer le nom d'un fichier et le déplacer
//par exemple de tmp vers le path de l'utilisateur


/* VARIABLES GLOBALES */
FILE *in, *out;
char out_path[4096];


struct s_usr_instr{
        char code;
        char ch;
        int m;
        int v;
};
typedef struct s_usr_instr usr_instr;

/* read_usr
 *---------
 *      lit les instruction de l'utilisateur dans l'entrée standard
 *  */
int read_usr(usr_instr *i){
        //TODO
        return EXIT_SUCCESS;
}

/* interpret
 *----------
 * interprète l'instruction de l'utilisateur
 *  */
int interpret(usr_instr *i){
        //TODO
        return EXIT_SUCCESS;
}

/* editor
 *-------
 * boucle d'interraction avec l'utilisateur
 * */
int editor(FILE *in, FILE *out){
        int n;
        int ln;
        usr_instr instr;

        do {
          read_usr(&instr);
          interpret(&instr);
        } while (instr != 'E');

        return EXIT_SUCCESS;
}

/* parse_arg
 *----------
 * ouvre les fichiers, parse les options
 * */
int parse_arg(int argc, char** argv){
        return EXIT_SUCCESS;
}


int main(int argc, char* argv[]){
        parse_arg(argc,argv);
        editor(in,out);
        return EXIT_SUCCESS;
}
