RM = rm -f

ALLOBJS = *.o

ALLDRIVOBS = *.exe *.prova

OTHERS = *.c *.h *.dlg

all: plumber

plumber: dist-clean
	antlr -gt plumber.g
	dlg -ci parser.dlg scan.c
	g++ -std=c++11 -Wno-write-strings -o plumber plumber.c scan.c err.c -I/home/brutus729/Documents/FIB/LP/pccts/h
	$(RM) $(ALLOBJS)
	$(RM) $(ALLDRIVOBS)
	$(RM) $(OTHERS)

clear: dist-clean

clean:
	$(RM) $(ALLOBJS)

clean-others: clean
	$(RM) $(ALLDRIVOBS)
	$(RM) $(OTHERS)

dist-clean: clean-others
	$(RM) plumber