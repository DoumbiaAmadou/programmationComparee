#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>

#define PORT 6666

/*

programme permettant d'afficher le log d'une partie lorsque celle ci est terminée

le programme est un serveur tcp qui attend qu'un client lui envoie le log

 */

int s; //la socket
struct sockaddr_in a; //adresse de la socket
int d, l;
struct sockaddr_in c;
char buffer[256]; //le tampon dans lequel le serveur reçoit le message
int read_chars_number; //le nombre de caractères lus

void error(char *message) {
  //quitte le programme en cas d'erreur et affiche un message sur la sortie d'erreur
  fprintf(stderr, "%s\n", message);
  close(s);
  exit(EXIT_FAILURE);
}

void createSocket() {
  //creer une socket
  s = socket(PF_INET,SOCK_STREAM,0);
  if (s==-1) {
    error("socket problem");
  }
}

void sockAddrInit() {
  //initialise l'adresse de la socket
  bzero(&a, sizeof(a));
  a.sin_family = AF_INET;
  a.sin_port = htons(PORT);
  a.sin_addr.s_addr = htonl(INADDR_ANY);
}

void bindSocket() {
  //relie la socket à son adresse
  if(bind(s, (struct sockaddr *) &a, sizeof(a)) == -1) {
    error("blind problem");
  }
}

void socketListen() {
  //fait attendre la socket d'une connexion
  if(listen(s,0) == -1) {
    error("listen problem");
  }
}

void acceptSocket() {
  //le serveur accepte la connexion avec un client
  d = accept(s,(struct sockaddr *) &c, (socklen_t *) &l);
  if(d == -1) {
    error("accept problem");
  }
  shutdown(s,SHUT_WR);
}

void readMessage() {
  //lit le message reçu
  int i;
  while((read_chars_number=read(d,buffer,256))>0) {
    for(i=0; i<read_chars_number; i++){
      printf("%c",buffer[i]);
    }
    printf("\n");
  }
}

void closeSocket() {
  close(d);
  close(s);
}

int main(int argc,char *argv[]) {
   
  createSocket();
  sockAddrInit();
  bindSocket();
  socketListen();
  acceptSocket();
  readMessage();
  closeSocket();  

  return 0;
}
