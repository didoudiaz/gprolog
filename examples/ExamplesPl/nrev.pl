/*  The naive reverse benchmark */



q:-	statistics(runtime,[S|_]), 
	bench(250),
	statistics(runtime,[S1|_]), Y is S1-S,
	write('time : '), write(Y), nl.





nrev([],[]).

nrev([X|Rest],Ans):-
	nrev(Rest,L), app(L,[X],Ans).




app([],L,L).

app([X|L1],L2,[X|L3]):-
	app(L1,L2,L3).




bench(Count):-
	statistics(runtime,[T0|_]),
	dodummy(Count),
	statistics(runtime,[T1|_]),
	dobench(Count),
	statistics(runtime,[T2|_]),
	report(Count,T0,T1,T2).



dobench(Count):-
	data(List),
	repeat(Count),
	nrev(List,_),
	fail.

dobench(_).




dodummy(Count):-
	data(List),
	repeat(Count),
	dummy(List,_),
	fail.

dodummy(_).




dummy(_,_).




data(X):-
	data(X,30).


data([],0).

data([a|Y],N):-
	N > 0, 
	N1 is N-1, 
	data(Y,N1).




repeat(_N).

repeat(N):-
	N > 1, 
	N1 is N-1, 
	repeat(N1).




report(Count,T0,T1,T2) :-
	Time1 is T1-T0,
	Time2 is T2-T1,
	Time  is Time2-Time1,		/* Time spent on nreving lists */
        Lips is (496*Count*1000)//Time,
 	write(Lips), write(' lips for '), write(Count),
	write(' iterations taking '), write(Time),
	write(' msec ('),
	write(Time2-Time1), write(')'),
	nl.



:- initialization(q).
