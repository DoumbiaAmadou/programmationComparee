#include <unistd.h>

typedef struct _mem_dict {
  	void *addr;
  	size_t size;
  	int freed;
} mem_dict;

mem_dict *dict = NULL;
int dict_count = 0;

void *malloc(size_t size) {
  	void *return_ptr = sbrk(size);
  
  	if (dict == NULL)
    	dict = sbrk(1024 * sizeof(mem_dict)); 
  	dict[dict_count].addr = return_ptr;
 	dict[dict_count].size = size;
  	dict_count++;
  
  	return return_ptr;
}

// void *realloc(void *ptr, size_t size) {}

// void *calloc(size_t nelem, size_t elsize) {}

void free(void *ptr) {
    if (!dict)
        return;

    for (int i = 0; i < dict_count; i++ ) {
        if (dict[i].addr == ptr) {
            dict[i].freed = 1;
            return;
        }
    }
}