RM = rm -f

ALLOBJS = *.o

ALLDRIVOBS = *.exe *.prova

all: dist-clean plumber.exe

allNew: clear all

plumber.exe:
	antlr -gt plumber.g
	dlg -ci parser.dlg scan.c
	g++ -std=c++11 -Wno-write-strings -o plumber.exe plumber.c scan.c err.c -I/home/brutus729/Documents/FIB/LP/Pccts/h
	
clear: clean dist-clean

clean:
	$(RM) $(ALLOBJS)

dist-clean: clean
	$(RM) $(ALLDRIVOBS)
