#include <string.h>
#include <unistd.h>
#include <stdio.h>

// structure to correlate the old memory location to the old memory size
typedef struct _mem_dictionary mem_dictionary;
struct _mem_dictionary 
{
  void *addr;
  size_t size;
  int free;
};
//a pointer to the first element of the dictionary
mem_dictionary* dictionary = NULL;

//count of how many records are part of this structure
int dictionary_ct = 0;

void *malloc(size_t size)
{
  void *_ptr = sbrk(size);
  
  if (dictionary == NULL)
  	/* Note the use of sbrk() and not malloc(), since malloc() would create an infinite loop of calling malloc(). */
  	dictionary = sbrk(1024 * sizeof(mem_dictionary)); 

  dictionary[dictionary_ct].addr = _ptr;

  dictionary[dictionary_ct].size = size;

  dictionary_ct++;
  
  return _ptr;
}

void free(void *ptr) {
  if (!ptr) {
    return;
  }
  int i;
  for (i = 0; i < dictionary_ct; i++ ) {
        if (dictionary[i].addr == ptr) {
            dictionary[i].free = 1;
            return;
        }
  }
}

// Test hello
 int main(void)
 {
    char *str;

    /* allocate memory for string */
    if ((str = (char *) malloc(10)) == NULL)
    {
       printf("Not enough memory to allocate buffer\n");
       return 0;  /* terminate program if out of memory */
    }

    /* copy "Hello" into string */
    strcpy(str, "Hello");

    /* display string */
    printf("String is %s\n", str);

    /* free memory */
    free(str);

    return 0;
 }
