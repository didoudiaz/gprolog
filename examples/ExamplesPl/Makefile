GPLC     = gplc
GPLCFLAGS= --fast-math --min-bips
BENCH_PL = boyer browse cal chat_parser crypt ham meta_qsort nand nrev \
           poly_10 qsort queens queensn query reducer sdda sendmore \
           tak tak_gvar zebra

.SUFFIXES:
.SUFFIXES: .pl $(SUFFIXES)



all: $(BENCH_PL)

clean:
	rm -f $(BENCH_PL) *.exe


boyer: boyer.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o boyer boyer.pl

browse: browse.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o browse browse.pl

cal: cal.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o cal cal.pl

chat_parser: chat_parser.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o chat_parser chat_parser.pl

crypt: crypt.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o crypt crypt.pl

ham: ham.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o ham ham.pl

meta_qsort: meta_qsort.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o meta_qsort meta_qsort.pl

nand: nand.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o nand nand.pl

nrev: nrev.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o nrev nrev.pl

poly_10: poly_10.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o poly_10 poly_10.pl

qsort: qsort.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o qsort qsort.pl

queens: queens.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o queens queens.pl

queensn: queensn.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o queensn queensn.pl

query: query.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o query query.pl

reducer: reducer.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o reducer reducer.pl

sdda: sdda.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o sdda sdda.pl

sendmore: sendmore.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o sendmore sendmore.pl

tak: tak.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o tak tak.pl

tak_gvar: tak_gvar.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o tak_gvar tak_gvar.pl

zebra: zebra.pl common.pl hook.pl
	$(GPLC) $(GPLCFLAGS) -o zebra zebra.pl
