GPLC     = gplc
GPLCFLAGS= --min-size
BENCH_FD=alpha bridge cars crypta digit8 donald eq10 eq20 five gardner magic\
         multipl partit queens send square send srq magsq qg5 langford interval

BENCH_BOOL=bdiag bdonald bpigeon bqueens bramsey bschur bsend


.SUFFIXES:
.SUFFIXES: .pl $(SUFFIXES)


all: fd bool

clean: rm-fd rm-bool
	rm -f *.exe


fd: $(BENCH_FD)


rm-fd:
	rm -f $(BENCH_FD)


alpha: alpha.pl
	$(GPLC) $(GPLCFLAGS) -o alpha alpha.pl

bridge: bridge.pl
	$(GPLC) $(GPLCFLAGS) -o bridge bridge.pl

cars: cars.pl
	$(GPLC) $(GPLCFLAGS) -o cars cars.pl

crypta: crypta.pl
	$(GPLC) $(GPLCFLAGS) -o crypta crypta.pl

digit8: digit8.pl
	$(GPLC) $(GPLCFLAGS) -o digit8 digit8.pl

donald: donald.pl
	$(GPLC) $(GPLCFLAGS) -o donald donald.pl

eq10: eq10.pl
	$(GPLC) $(GPLCFLAGS) -o eq10 eq10.pl

eq20: eq20.pl
	$(GPLC) $(GPLCFLAGS) -o eq20 eq20.pl

five: five.pl
	$(GPLC) $(GPLCFLAGS) -o five five.pl

gardner: gardner.pl
	$(GPLC) $(GPLCFLAGS) -o gardner gardner.pl

partit: partit.pl
	$(GPLC) $(GPLCFLAGS) -o partit partit.pl

magic: magic.pl
	$(GPLC) $(GPLCFLAGS) -o magic magic.pl

multipl: multipl.pl
	$(GPLC) $(GPLCFLAGS) -o multipl multipl.pl

queens: queens.pl queens_fd.fd
	$(GPLC) $(GPLCFLAGS) -o queens queens.pl queens_fd.fd

send: send.pl
	$(GPLC) $(GPLCFLAGS) -o send send.pl

square: square.pl
	$(GPLC) $(GPLCFLAGS) -o square square.pl

srq: srq.pl
	$(GPLC) $(GPLCFLAGS) -o srq srq.pl

magsq: magsq.pl
	$(GPLC) $(GPLCFLAGS) -o magsq magsq.pl

qg5: qg5.pl
	$(GPLC) $(GPLCFLAGS) -o qg5 qg5.pl

langford: langford.pl
	$(GPLC) $(GPLCFLAGS) -o langford langford.pl

interval: interval.pl
	$(GPLC) $(GPLCFLAGS) -o interval interval.pl





bool: $(BENCH_BOOL)

rm-bool:
	rm -f $(BENCH_BOOL)



bdiag: bdiag.pl array.pl
	$(GPLC) $(GPLCFLAGS) -o bdiag bdiag.pl

bdonald: bdonald.pl array.pl
	$(GPLC) $(GPLCFLAGS) -o bdonald bdonald.pl

bpigeon: bpigeon.pl array.pl
	$(GPLC) $(GPLCFLAGS) -o bpigeon bpigeon.pl

bqueens: bqueens.pl array.pl
	$(GPLC) $(GPLCFLAGS) -o bqueens bqueens.pl

bramsey: bramsey.pl array.pl
	$(GPLC) $(GPLCFLAGS) -o bramsey bramsey.pl

bschur: bschur.pl array.pl
	$(GPLC) $(GPLCFLAGS) -o bschur bschur.pl

bsend: bsend.pl array.pl
	$(GPLC) $(GPLCFLAGS) -o bsend bsend.pl

