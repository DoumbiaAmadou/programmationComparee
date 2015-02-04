#include <unistd.h>

/**
 * Enregistrement des blocs alloués
**/
typedef struct alloc{
  void* ptr;
  size_t len;
  int in_use;
} alloc;

alloc allocs[10000];

void *malloc(size_t size){
  int i;
  for(i=0; i<10000;i++){
    if(allocs[i].len == -1) break;
    if(size <= allocs[i].len && !allocs[i].in_use){
      allocs[i].in_use = 1;
      return allocs[i].ptr;
    }
  }
  if(i==9999) return NULL;
  void *p = sbrk(size);
  allocs[i].in_use = 1;
  allocs[i].len = size;
  allocs[i].ptr = p;
  allocs[i+1].len = -1;
  return p;
}

void free(void* p){
  int i;
  for(i=0;i<10000;i++)
    if(allocs[i].ptr = p) break;
  allocs[i].in_use = 0;
}
