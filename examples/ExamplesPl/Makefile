GPLC     = gplc
GPLCFLAGS= --fast-math --min-size
BENCH_PL = boyer browse cal chat_parser crypt ham meta_qsort nand nrev \
           poly_10 queens queens8 reducer sdda sendmore tak tak_gvar zebra

.SUFFIXES:
.SUFFIXES: .pl $(SUFFIXES)



all: $(BENCH_PL)

clean:
	rm -f $(BENCH_PL) *.exe




boyer: boyer.pl
	$(GPLC) $(GPLCFLAGS) -o boyer boyer.pl

browse: browse.pl
	$(GPLC) $(GPLCFLAGS) -o browse browse.pl

cal: cal.pl
	$(GPLC) $(GPLCFLAGS) -o cal cal.pl

chat_parser: chat_parser.pl
	$(GPLC) $(GPLCFLAGS) -o chat_parser chat_parser.pl

crypt: crypt.pl
	$(GPLC) $(GPLCFLAGS) -o crypt crypt.pl

ham: ham.pl
	$(GPLC) $(GPLCFLAGS) -o ham ham.pl

meta_qsort: meta_qsort.pl
	$(GPLC) $(GPLCFLAGS) -o meta_qsort meta_qsort.pl

nand: nand.pl
	$(GPLC) $(GPLCFLAGS) -o nand nand.pl

nrev: nrev.pl
	$(GPLC) $(GPLCFLAGS) -o nrev nrev.pl

poly_10: poly_10.pl
	$(GPLC) $(GPLCFLAGS) -o poly_10 poly_10.pl

queens: queens.pl
	$(GPLC) $(GPLCFLAGS) -o queens queens.pl

queens8: queens8.pl
	$(GPLC) $(GPLCFLAGS) -o queens8 queens8.pl

reducer: reducer.pl
	$(GPLC) $(GPLCFLAGS) -o reducer reducer.pl

sdda: sdda.pl
	$(GPLC) $(GPLCFLAGS) -o sdda sdda.pl

sendmore: sendmore.pl
	$(GPLC) $(GPLCFLAGS) -o sendmore sendmore.pl

tak: tak.pl
	$(GPLC) $(GPLCFLAGS) -o tak tak.pl

tak_gvar: tak_gvar.pl
	$(GPLC) $(GPLCFLAGS) -o tak_gvar tak_gvar.pl

zebra: zebra.pl
	$(GPLC) $(GPLCFLAGS) -o zebra zebra.pl
