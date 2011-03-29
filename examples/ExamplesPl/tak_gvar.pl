% generated: 17 November 1989
% option(s): SOURCE_TRANSFORM_1
%
%   tak
%
%   Evan Tick (from Lisp version by R. P. Gabriel)
%
%   (almost) Takeuchi function (recursive arithmetic)


% uses global variables to tabulate results


tak_gvar(ShowResult) :-
	init_tak_array,
	tak(18,12,6,R),
	(   ShowResult = true ->
	    write(tak(18,12,6)=R), nl
	;   true).

init_tak_array:-
	g_assign(tak,g_array_auto(20,g_array_auto(20,g_array_auto(20,null)))).



tak(X,Y,Z,A):-
	g_read(tak(X,Y,Z),A1),
	(integer(A1) -> A=A1
                     ;  tak1(X,Y,Z,A),
		        g_assign(tak(X,Y,Z),A)).


tak1(X,Y,Z,A):-
        X =< Y,
        Z = A.

tak1(X,Y,Z,A):-
	X > Y,
        X1 is X - 1,
        tak(X1,Y,Z,A1),
        Y1 is Y - 1,
        tak(Y1,Z,X,A2),
        Z1 is Z - 1,
        tak(Z1,X,Y,A3),
        tak(A1,A2,A3,A).

% benchmark interface

benchmark(ShowResult) :-
	tak_gvar(ShowResult).

:- include(common).

