TESTEDIR := bateria1
TARGET := main
EXE := $(TARGET).exe
SAIDA := saida.txt
TESTEARQ := $(filter-out $(TESTEDIR)/saida.txt, $(wildcard $(TESTEDIR)/*.txt))

all: build test clean

build:
	ghc main.hs -no-keep-hi-files -no-keep-o-files

test:
	cp $(TESTEARQ) .
	echo ''
	.\$(EXE)

clean:
	echo ''
	rm -f *.txt
	rm -f $(EXE)	