GHC		= ghc
SRC		= src
TEMPLATES	= templates
MAIN		= Main.hs
BINARY		= gisli.hamstur.is
LD_LIBRARY_PATH	= /opt/local/lib:/Developer/SDKs/MacOSX10.3.9.sdk/usr/lib

export LD_LIBRARY_PATH

all: 	$(SRC)/$(MAIN)
	ghc -i$(SRC) --make -o $(BINARY) $(SRC)/$(MAIN)

build:	$(SRC)/$(MAIN)
	ghc -i$(SRC) -c $(SRC)/$(MAIN)

clean:	$(SRC)/*.o  $(SRC)/*/*.o
	find $(SRC) -type f -name "*.o" -exec rm {} \;

clean-bin:	$(BINARY)
		rm $(BINARY)

clean-haskell:	$(SRC)/*.hi $(SRC)/*/*.hi
		find $(SRC) -type f -name "*.hi" -exec rm {} \;

localhost:	$(TEMPLATES)/layout.st
		sed -e 's/gisli-hamstur-is/localhost-hamstur-is/g;s/gisli.hamstur.is/127.0.0.1:5000/g' -i.bak $(TEMPLATES)/layout.st

gisli:		$(TEMPLATES)/layout.st
		sed -e 's/localhost-hamstur-is/gisli-hamstur-is/g;s/127.0.0.1:5000/gisli.hamstur.is/g' -i.bak $(TEMPLATES)/layout.st

