struct list_c
{
   struct list_c *pre;
   size_t size;
   bool isFree;
};

static list_c *list = NULL;
void *malloc(size_t size)
{
   list_c *c = sbrk(sizeof(*c) + size);
 
   c->pre = NULL;
   c->size = size;
   c->isFree = true;
   if (list == NULL)
     list->pre = c;
   list = c; 
   return c + 1; 
 }
void free(void *p)
{
   list_c *c = p;
   c -= 1;
   c->isFree = false;
}
