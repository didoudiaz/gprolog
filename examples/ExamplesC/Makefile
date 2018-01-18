GPLC = gplc
#CFLAGS='-Wall -g'
CFLAGS=-O


EXECS=examp new_main

all: $(EXECS)

examp: examp.pl examp_c.c
	$(GPLC) --new-top-level -C '$(CFLAGS)' examp.pl examp_c.c

new_main: new_main.pl new_main_c.c
	$(GPLC) -C '$(CFLAGS)' new_main.pl new_main_c.c

clean:
	rm -f $(EXECS) *.exe



