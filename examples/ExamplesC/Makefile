GPLC = gplc

EXECS=examp new_main

all: $(EXECS)

examp: examp.pl examp_c.c
	$(GPLC) examp.pl examp_c.c

new_main: new_main.pl new_main_c.c
	$(GPLC) new_main.pl new_main_c.c

clean:
	rm -f $(EXECS) *.exe



