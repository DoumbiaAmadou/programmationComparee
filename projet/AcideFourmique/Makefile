SRCDIR=src/main/scala/gameLog
FLAGS=-Wall
CC=gcc
exe=logPrinter


compile: $(exe)
	sbt compile

run: $(exe)
	./$(exe) &
	sbt run

$(exe): LogPrinter.o
	$(CC) -o $(exe) $<

LogPrinter.o: $(SRCDIR)/LogPrinter.c
	$(CC) $(FLAGS) -c $<

cleanAll: clean
	rm -rf target/ project/

clean:
	rm -f LogPrinter.o $(exe)
