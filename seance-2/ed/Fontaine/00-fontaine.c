/* fontaine's ed version 0 - codename "Don't Reinvent The Wheel" */
#include <stdio.h>
#include <unistd.h>

int main(int argc, char **argv) {
        return execvp("ed", argv);
}
