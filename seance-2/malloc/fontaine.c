#include <unistd.h>

/**
 * A dead-simple malloc/free implementation, using only sbrk(2). It works as a
 * stack; you can only free the last malloc'ed pointer.
 *
 * Note: the `bf_free` function doesn't work for now.
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
 * - It adds a `sizeof(size_t)` overhead to each pointer
 * - It prevents pointers arithmetic since malloc-ed spaces are not contiguous
 *   (there're `sizeof(size_t)` bytes between each addressable memory space)
 * - It doesn't reuse previously malloc-then-freed space
 *
 * Advantages:
 * - This implementation can virtually malloc memory up to the limit set by the
 *   OS while `didier.c` has an hardcoded limit.
 *
 * A better implementation would use mmap(2) with a register.
 **/

#define BF_SIZESIZE (sizeof (size_t))

#define BF_SIZEOF(p) \
        (*(((size_t*)p) - 1))

void *bf_malloc(size_t size) {
        void *p = sbrk(BF_SIZESIZE + size);
        if (p == (void*)-1) { return NULL; }
        *(size_t*)p = size;
        return (void*)((size_t*)p + 1);
}

int bf_free(void *p) {
        // doesn't work, it doesn't free space. We should keep a pointer to the
        // currently available space
        void *ret = sbrk(-BF_SIZEOF(p) - BF_SIZESIZE);
        // yes it could be shortened but this expression is clearer for the
        // programmer
        return (ret == (void*)-1) ? 1 : 0;
}

// simple test
int main(void) {
        int *p1 = bf_malloc(sizeof(int)),
            *p2 = bf_malloc(sizeof(int)),
            *p3;

        if (p1 == NULL || p2 == NULL) { return 1; }

        if (bf_free(p2) != 0 || bf_free(p1) != 0) { return 2; }

        p3 = bf_malloc(sizeof(int));

        if (p1 != p3) { return 3; }

        if (bf_free(p3) != 0) { return 4; }

        return 0;
}
