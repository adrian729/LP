RM = rm -f

ALLOBJS = *.o

ALLDRIVOBS = *.exe *.prova

all: dist-clean exam.exe

allNew: clear all

exam.exe:
	antlr -gt exam.g
	dlg -ci parser.dlg scan.c
	g++ -std=c++11 -Wno-write-strings -o exam.exe exam.c scan.c err.c -I/home/brutus729/Documents/FIB/LP/pccts/h

clear: clean dist-clean

clean:
	$(RM) $(ALLOBJS)

dist-clean: clean
	$(RM) $(ALLDRIVOBS)