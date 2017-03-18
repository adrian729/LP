RM = rm -f

ALLOBJS = *.o

ALLDRIVOBS = *.exe *.prova *.c *.h *.dlg

all: dist-clean example0.exe

allNew: clear all

example0.exe:
	antlr example0.g
	dlg parser.dlg scan.c
	gcc -Wno-write-strings -o example0.exe example0.c scan.c err.c -I../../../Pccts/h

clear: clean dist-clean

clean:
	$(RM) $(ALLOBJS)

dist-clean: clean
	$(RM) $(ALLDRIVOBS)
