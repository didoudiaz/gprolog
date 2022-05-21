% crypt
%
% Cryptomultiplication:
% Find the unique answer to:
%	OEE
%	 EE
% 	---
%      EOEE
%      EOE
%      ----
%      OOEE
%
% where E=even, O=odd.
% This program generalizes easily
% to any such problem.
% Written by Peter Van Roy




crypt(ShowResult) :-
	odd(A), even(B), even(C), even(E),
	mult([C, B, A], E, [I, H, G, F | X]),
	lefteven(F), odd(G), even(H), even(I), zero(X), lefteven(D),
	mult([C, B, A], D, [L, K, J | Y]),
	lefteven(J), odd(K), even(L), zero(Y),
	sum2([I, H, G, F], [0, L, K, J], [P, O, N, M | Z]),
	odd(M), odd(N), even(O), even(P), zero(Z),
	(   ShowResult = true ->
	    write(' '), write(A), write(B), write(C), nl,
	    write('  '), write(D), write(E), nl,
	    write(F), write(G), write(H), write(I), nl,
	    write(J), write(K), write(L), nl,
	    write(M), write(N), write(O), write(P), nl
	;   true).

% In the usual source this predicate is named sum. However, sum is a
% language construct in NU-Prolog, and cannot be defined as a predicate.
% If you try, nc comes up with an obscure error message.

sum2(AL, BL, CL) :-
	sum2(AL, BL, 0, CL).

sum2([A | AL], [B | BL], Carry, [C | CL]) :- !,
	X is (A + B + Carry),
	C is X mod 10,
	NewCarry is X // 10,
	sum2(AL, BL, NewCarry, CL).
sum2([], BL, 0, BL) :- !.
sum2(AL, [], 0, AL) :- !.
sum2([], [B | BL], Carry, [C | CL]) :- !,
	X is B + Carry,
	NewCarry is X // 10,
	C is X mod 10,
	sum2([], BL, NewCarry, CL).
sum2([A | AL], [], Carry, [C | CL]) :- !,
	X is A + Carry,
	NewCarry is X // 10,
	C is X mod 10,
	sum2([], AL, NewCarry, CL).
sum2([], [], Carry, [Carry]).

mult(AL, D, BL) :- mult(AL, D, 0, BL).

mult([], _, Carry, [C, Cend]) :-
	C is Carry mod 10,
	Cend is Carry // 10.
mult([A | AL], D, Carry, [B | BL] ) :-
	X is A * D + Carry,
	B is X mod 10,
	NewCarry is X // 10,
	mult(AL, D, NewCarry, BL).

zero([]).
zero([0 | L]) :- zero(L).

odd(1).
odd(3).
odd(5).
odd(7).
odd(9).

even(0).
even(2).
even(4).
even(6).
even(8).

lefteven(2).
lefteven(4).
lefteven(6).
lefteven(8).


% benchmark interface

benchmark(ShowResult) :-
	crypt(ShowResult).

:- include(common).
