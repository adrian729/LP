RM = rm -f

ALLOBJS = *.o

ALLDRIVOBS = *.exe *.prova *.c *.h *.dlg

all: dist-clean example1.exe

allNew: clear all

example1.exe:
	antlr -gt example1.g
	dlg -ci parser.dlg scan.c
	g++ -Wno-write-strings -o example1.exe example1.c scan.c err.c -I../../../Pccts/h

clear: clean dist-clean

clean:
	$(RM) $(ALLOBJS)

dist-clean: clean
	$(RM) $(ALLDRIVOBS)
