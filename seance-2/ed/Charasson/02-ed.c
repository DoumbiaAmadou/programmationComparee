#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define EXIT_ON_ERR(msg, size, err_num){\
	write(STDERR_FILENO,msg,size);\
	return err_num;\
}

#define ERR_MSG(msg, size){\
	write(STDERR_FILENO,msg,size);\
}

#define BUF_SIZE 256
#define INSTR_ERR 0
#define INSTR_EXIT 2
#define INSTR_CONT 4


/* VARIABLES GLOBALES */
int in, out;
char out_path[512];


struct s_usr_instr{
	char code;
	int m;
	int n;
	char *txt;
};
typedef struct s_usr_instr usr_instr;

/* 
 *
 * retourne la position de la première virgule dans la chaine
 * si il n'y en a plus retourne -1
 *
 * */
int next_comma(char *str){
	int i;
	for(i=0 ; str[i]!='\0' ; i++){
		if(str[i]==','){
			return i;
		}
	}
	return -1;
}

/* read_usr
 *---------
 *	lit les instruction de l'utilisateur dans l'entrée standard
 *  */
int read_usr(char *buf){
	int n;
	n = read(STDIN_FILENO,buf,BUF_SIZE);
	
	if(n > BUF_SIZE){
		EXIT_ON_ERR("error : instruction is bigger than 255\n\0",40,EXIT_FAILURE);
		return EXIT_FAILURE;
	}	
	
	buf[n-1]='\0';
	return EXIT_SUCCESS; 	
}



int atoi2(char *s, int size){	
	if( size <0)
		EXIT_ON_ERR("error : <atoi> bad size\0",24,INSTR_ERR);	
	
	int j, num=0, pow=1;
	for(j = size-1 ; j>=0 ; j--){
		if((s[j] < '0') || (s[j] > '9'))
			EXIT_ON_ERR("error : <atoi> not an interger\0",31,INSTR_ERR);
		num += (s[j]-'0')*pow;
		pow*=10;
	}
	return num;
}

/* read_instruction
 *----------
 * découpe l'instruction de l'utilisateur
 * pour les récupérer dans la structure usr_instr "i"
 *
 * retourne EXIT_FAILURE en cas d'échec
 *  */
int read_instruction(char *s_usr,usr_instr *i){
	int c;
	char err[]="error : malformed instruction\n\0";//size = 30
	char *s = s_usr;

	i->code = s[0];

	/* instruction E */
	if(i->code=='E'){
		return INSTR_EXIT;
	}

	/* recherche m */
	c = next_comma(s);//1ere virgule
	if(c != 1)
		EXIT_ON_ERR(err,30,INSTR_ERR);
	s += c+1;
	
	/* calcule du chiffre m*/
	c = next_comma(s);//2eme virgule
	if(c == -1)
		EXIT_ON_ERR(err,30,INSTR_ERR);
	s+=c+1;
	i->m = atoi2(s,c);
	
	/* instruction I */
	if(i->code == 'I'){
		i->txt = s;
		return INSTR_CONT;
	}

	/* calcule du chiffre n */
	c = next_comma(s);//3eme virgule
	if(c == -1)
		EXIT_ON_ERR(err,30,INSTR_ERR);
	s+=c+1;
	i->n = atoi2(s,c);

	/* instruction R D et autres */
	switch(i->code){	
		case 'R':
			i->txt = s;
			return INSTR_CONT;
		break;

		case 'D':
			return INSTR_CONT;
		break;

		default:
			EXIT_ON_ERR(err,30,INSTR_ERR);
	}

	return EXIT_SUCCESS;
}

/* editor
 *-------
 * boucle d'interraction avec l'utilisateur
 * */
int editor(){
	int ret;
	char buf[BUF_SIZE];
	

	do{
		read_usr(buf);
		//ret = read_instruction("TODO",NULL);
		if(ret == INSTR_ERR){
			return EXIT_FAILURE;
		}
	}while( ret != INSTR_CONT);

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
	/* parsing des arguments */
	if(argc<3){
		EXIT_ON_ERR("error : missing arguments\n\0",27,EXIT_FAILURE);
	}

	if((in = open(argv[1],O_RDONLY)) == -1){
		EXIT_ON_ERR("error : Cannot open file\n\0",26,EXIT_FAILURE);
	}
	if((out = open(argv[2],O_CREAT|O_WRONLY,S_IRWXU)) == -1){
		EXIT_ON_ERR("error : Cannot create out file\n\0",32,EXIT_FAILURE);
	}
	
	//editor();
	/* teste de next comma */
	/*  
	char te[]="ceci est , du texte, avec, des virgules\0";
	printf("le texte : <%s>\n",te);
	int ii;
	printf("la première virgule ce trouve à la position %d\n",(ii=next_comma(te)));

	char *tete = te;
	tete+=ii;
	printf("le reste de la chaine est <%s>\n",tete);
	*/
	/* teste de read_usr */
	char buf[BUF_SIZE];
	read_usr(buf);
	printf("b = %s\n",buf);

	/* teste de read_instruction*/
	usr_instr i;
	read_instruction(buf,&i);
	printf("usr_instr contient les infos suivante: %c/%d/%s\n",i.code,i.m,i.txt);


	return EXIT_SUCCESS;
}
