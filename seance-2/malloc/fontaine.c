#include <stdio.h>
#include <unistd.h>

/**
 * A dead-simple malloc/free implementation, using only sbrk(2). It works as a
 * stack; you can only free the last malloc'ed pointer.
 *
 * This could be circumvented by using a global register, but didier.c
 * already did it and it might be interesting to try other ways.
 *
 * It works by prepending `sizeof(size_t)` bytes to any malloc-ed pointer. The
 * size of the memory pointed out by the pointer is stored in these bytes.
 * `bf_free` can then reduce the used memory by decrementing the data space
 * using this size.
 *
 * Limitations:
 * - It works as a stack: you can only free the last malloc-ed space, or it'll
 *   break the program
 * - There's nothing preventing the user from writing outside of a malloc-ed
 *   memory space if there's something after it, e.g.:
 *
 *      int *p1 = bf_malloc(sizeof(int)),
 *          *p2 = bf_malloc(sizeof(int));
 *
 *      *(p1 + 3) = 42;
 *      printf("%d\n", *p2); // 42
 *
 * - It works with a contiguous memory space only
 *
 * A better implementation would use mmap(2) with a thread-proof register.
 **/

#define BF_SIZESIZE (sizeof (size_t))

#define BF_SIZEOF(p) \
        (*(((size_t*)p) - 1))

void *bf_malloc(size_t size) {
        void *p = sbrk(BF_SIZESIZE + size);
        *(size_t*)p = size;
        return (void*)((size_t*)p + 1);
}

// FIXME doesn't work (sbrk fails)
int bf_free(void *p) {
        void *ret = sbrk(-(BF_SIZEOF(p) + BF_SIZESIZE));
        return (ret != (void*)-1);
}

int main(void) {
        int *p1 = bf_malloc(sizeof(int)),
            *p2 = bf_malloc(sizeof(int));

        printf("p1=%p p2=%p\n", p1, p2);

        *p1 = 42;
        *p2 = 17;

        if (bf_free(p2) != 0) { puts("Error when freeing p2"); }
        if (bf_free(p1) != 0) { puts("Error when freeing p1"); }

        p1 = bf_malloc(sizeof(int));

        printf("p1=%p\n", p1);

        bf_free(p1);

        return 0;
}
