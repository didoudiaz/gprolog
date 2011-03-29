/*  The naive reverse benchmark */



nrev(ShowResult) :-
	bench(2500, ShowResult).



nrev([],[]).

nrev([X|Rest],Ans):-
	nrev(Rest,L),
	append(L,[X],Ans).




my_append([],L,L).

my_append([X|L1],L2,[X|L3]):-
	my_append(L1,L2,L3).


/* commented since it is defined in common.pl
get_cpu_time(T) :-
	statistics(runtime,[T|_]).
*/


bench(Count, ShowResult):-
	get_cpu_time(T0),
	dodummy(Count),
	get_cpu_time(T1),
	dobench(Count),
	get_cpu_time(T2),
	(   ShowResult = true ->
	    report(Count,T0,T1,T2)
	;   true).



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
        (Time2 =< Time1 ->
	    Time = 1
	;
	    Time is Time2-Time1	/* Time spent on nreving lists */
	),
        Lips is (496*Count*1000)//Time,
 	write(Lips), write(' lips for '), write(Count),
	write(' iterations taking '), write(Time),
	write(' msec ('),
	write(Time2-Time1), write(')'),
	nl.



% benchmark interface

benchmark(ShowResult) :-
	nrev(ShowResult).

:- include(common).

