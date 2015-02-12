CC=gcc
CFLAGS=-Wall
SRC=kilic-ed.c
OBJS=$(SRC:.c=.o)
EXEC=ed

all: $(EXEC)

$(EXEC):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

%.o:%.c 
	$(CC) $(CFLAGS) -o $@ -c $< 

clean:
	rm -f $(EXEC) $(OBJS) *~ 
