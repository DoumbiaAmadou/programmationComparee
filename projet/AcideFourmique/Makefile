SRCDIR=src/main/scala/gameLog
FLAGS=-Wall
CC=gcc
exe=$(SRCDIR)/logPrinter


compile: $(exe)
	sbt compile

$(exe): $(SRCDIR)/LogPrinter.o
	$(CC) -o $(exe) $<

$(SRCDIR)/LogPrinter.o: $(SRCDIR)/LogPrinter.c
	$(CC) $(FLAGS) -c $< -o $@

run: $(exe)
	./$(exe) &
	sbt run

clean:
	rm -f $(SRCDIR)/LogPrinter.o $(exe)
