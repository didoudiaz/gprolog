% Sdda3		5-Oct-86
% For use on simulator

%% To do:  (look for '%%')
%%	recursion - keep list of call procedures, ignore recursive calls
%%	   	problem: doesn't work for typical procedure working on a list,
%%			 since the list is smaller (different) each time.
%%		possible optimization: "recognize" base case & skip to it
%%	follow atoms, g is 'any atom', all others unique,  does it work?
%%	stats - write heapused, cputime to files (as comments)
%%	worst_case - handle ground terms (copy unify, modify atomic)
%%	handle disjunction - needs worst_case
%%	add cuts where possible to save space
%%	fill in rest of built-ins
%% 	how to handle op?
%%	Handle assert/retract?  call?  (If given ground terms- ok, vars- no)
%%		must have ground functor, definite number of args!



% Front end for simulator use
sdda(ShowResult):-
	do_sdda(test,_A,_B,_C, ShowResult).

% Does the sdda on FileName, instantiates Exitmodes to list of exit modes,
% ExitModes structure: [[Funtor/Arity, Activation, Exit], ... ],
% e.g. [[a/2, [g,X], [g,g]]
do_sdda(_FileName, ExitModes, _BackList, _PredList, ShowResult) :-
	%%see(FileName),
	read_procedures(Procs, ExitModes, Entries),	% collect all procedures
	%%seen,
	(   ShowResult = true ->
	    write('Procedures '), nl, write_list(Procs), nl,
	    write('Entry points '), nl, write_list(Entries), nl,
	    (nonvar(ExitModes) ->				% Don't mention there
		(write('Declared exit modes '), nl, 	% aren't any
		 write_list(ExitModes), nl) ;
		true),
	    entry_exit_modes_list(Procs, ExitModes, Entries),
	    write('Exit modes '), nl, write_list(ExitModes), nl
	;   true).

%%%  !!! Hard code in read for test:
%	sdda_entry(c(A,B,C)).
%	a(X, Y).
%	a(X, X).
%	c(A,B,C) :- a(A,B).

read_procedures([[a/2,a(_109,_110),a(_148,_148)|_184],
		 [c/3,(c(_191,_192,_193):-a(_191,_192))|_238]|_239],
		 _68,[c(_76,_77,_78)|_102]) :- !.

% For each entry point in Entries do sdda, building Known, an unbound-tail list
% Known structure: [[Name/Arity, ActivationModes, ExitModes], ...|_],
% where ActivationModes and ExitModes are lists of variables and the atom 'g'.
% 'g' represents a ground element and variables represent equivalence classes.
entry_exit_modes_list(_, _, Entries) :-			% Done
	var(Entries).
entry_exit_modes_list(ProcList, Known, [Entry|Entries]) :-
	Entry =.. [Functor|Act],		% Get functor/arity & activation
	my_length(Act, Arity),			% from entry declaration
	proc_exit_mode(ProcList, Known, [], Functor/Arity, Act, _),  % No invoc.
	entry_exit_modes_list(ProcList, Known, Entries).

% Do sdda on procedure Functor/Arity, given activation mode Act.  Instantiates
% Known to known exit modes and Act to exit modes for Functor/Arity under Act
proc_exit_mode(_, _, _, Functor/Arity, Act, Exit) :-
	built_in(Functor/Arity, Act, Exit).		    % This is a built-in
proc_exit_mode(_, Known, _, Functor/Arity, Act, Exit) :-
	look_up_act([Functor/Arity, Act, Exit], Known).       % Already did this
proc_exit_mode(ProcList, Known, Invocations, Functor/Arity, Act, Exit) :-
	umember([Functor/Arity|Clauses], ProcList),	% Look up definition
	dup(Clauses, ClausesCopy),			% Don't munge original
	clause_exit_modes_list(ProcList, Known, Invocations,
			       ClausesCopy, Act, Exits),
	(Exits=[] -> fail ; true),		       % didn't find any => fail
	worst_case(Exits, Exit),			% assume the worst
	dup(Act, ActCopy),				% Need copy because Body
	add_to_list([Functor/Arity, ActCopy, Exit], Known).   % binds Act & Exit
proc_exit_mode(_, Known, _, Functor/Arity, Act, Exit) :-
	write('No such procedure at compile time '),
	Activation=..[Functor|Act],
	write(Activation), nl,
	all_shared(Act, Exit),     	   % return worst possible - all shared
	add_to_list([Functor/Arity, Act, Exit], Known).

my_length(L, N) :-
        my_length1(L, 0, N).


my_length1([], N, N).

my_length1([_|L], M, N) :-
        M1 is M+1,
        my_length1(L, M1, N).

% Analyze all clauses for this procedure, instantiate Exits to all exit modes
clause_exit_modes_list(_, _, _, Clauses, _, []) :-
	var(Clauses), !.			       % No more clauses => done
clause_exit_modes_list(ProcList, Known, Invocations,
		       [Clause|Clauses], Act, Exits) :-
	eqmember([Clause, Act], Invocations), 		% This is a recursive
    write('skipping clause exit mode for '),
    write(Clause), write(' '), write(Act), nl,
	clause_exit_modes_list(ProcList, Known, Invocations,	% call, ignore
			       Clauses, Act, Exits).		% it
clause_exit_modes_list(ProcList, Known, Invocations,
		       [Clause|Clauses], Act, [Exit|Exits]) :-
	dup(Act, Exit),					% We'll bind Exit
	clause_exit_mode(ProcList, Known, [[Clause, Act]|Invocations],
		 	 Clause, Exit),			% Record invocation
	clause_exit_modes_list(ProcList, Known, Invocations,
		     	       Clauses, Act, Exits).
clause_exit_modes_list(ProcList, Known, Invocations,
		       [_Clause|Clauses], Act, Exits) :- 	% Unify failed
	clause_exit_modes_list(ProcList, Known, Invocations,
			       Clauses, Act, Exits).

% Given activation modes for this clause, return its exit modes
clause_exit_mode(ProcList, Known, Invocations, Clause, Act) :-
	(Clause = ':-'(Head, Body) ; Clause=Head, Body=true),	% Decompose it
	Head =.. [_|Args],					% Bind the head
	unify(Args, Act),					% to activation
	body_exit_mode(ProcList, Known, Invocations, Body). 	% do the body

body_exit_mode(ProcList, Known, Invocations, ','(Goal, Goals)) :-  % Conjunction
	body_exit_mode(ProcList, Known, Invocations, Goal),	% Do 1st
	body_exit_mode(ProcList, Known, Invocations, Goals).	% & rest
body_exit_mode(ProcList, Known, Invocation, Goal) :-
	functor(Goal, Functor, Arity),
	Goal =.. [Functor|Act],
	proc_exit_mode(ProcList, Known, Invocation, Functor/Arity, Act, Exit),
	unify(Act, Exit).

% Unifies Left and Right with the special case that the atom 'g' matches
% any atom (except [])
unify(Left, Left) :- !.				% Try standard unify first
unify(Left, g) :-				% else, is it special case
	atomic(Left), !,
	\+ Left=[].
unify(g, Right) :-
	atomic(Right), !,
	\+ Right=[].
unify([LeftHead|LeftTail], [RightHead|RightTail]) :-	% or list
	!, unify(LeftHead, RightHead),
	unify(LeftTail, RightTail).
unify(Left, Right) :-					% or structure
	Left =.. [Functor|LeftArgs],
	Right =.. [Functor|RightArgs],
	unify(LeftArgs, RightArgs).

% Succeed if Left and Right are equivalent, i.e. they are the exact same
% with variables renamed
equiv(Left, Right) :-
	equiv(Left, Right, _).
equiv(Left, Right, _) :-
	Left==Right, !.
equiv(g, Right, _) :-
	atomic(Right), !,
	\+ Right=[].
equiv(Left, g, _) :-
	atomic(Left), !,
	\+ Left=[].
equiv(Left, Right, Bindings) :-
	var(Left), !,
	var(Right),
	equiv_vars(Left, Right, Bindings).
equiv(Left, Right, Bindings) :-
	var(Right), !,
	var(Left),
	equiv_vars(Left, Right, Bindings).
equiv([LeftHead|LeftTail], [RightHead|RightTail], Bindings) :-
	!, equiv(LeftHead, RightHead, Bindings),
	equiv(LeftTail, RightTail, Bindings).
equiv(Left, Right, Bindings) :-
	Left=..[Functor|LeftArgs],
	Right=..[Functor|RightArgs],
	equiv(LeftArgs, RightArgs, Bindings).

equiv_vars(Left, Right, Bindings) :-
	var(Bindings), !,
	Bindings=[[Left, Right]|_].
equiv_vars(Left, Right, [[AnyVar, AnyBinding]|_]) :-
	Left==AnyVar, !,
	Right==AnyBinding.
equiv_vars(Left, Right, [[AnyVar, AnyBinding]|_]) :-
	Right==AnyBinding, !,
	Left==AnyVar.
equiv_vars(Left, Right, [ _|Bindings]) :-
	equiv_vars(Left, Right, Bindings).

% Make a copy of Orig with new vars.  Copy must be a variable.
% E.g. dup([A,s(A,B),[B,C]], New) binds New to [X,s(X,Y),[Y,Z]]
dup(Orig, Copy) :-
	dup(Orig, Copy, _).
dup(Orig, Copy, Bindings) :-
	var(Orig), !,
	dup_var(Orig, Copy, Bindings).
dup(Orig, Orig, _) :-				% Atoms, including []
	atomic(Orig), !.
dup([OrigHead|OrigTail], [CopyHead|CopyTail], Bindings) :-
	!, dup(OrigHead, CopyHead, Bindings),
	dup(OrigTail, CopyTail, Bindings).
dup(Orig, Copy, Bindings) :-
	Orig=..[Functor|OrigArgs],
	dup(OrigArgs, CopyArgs, Bindings),
	Copy=..[Functor|CopyArgs].

dup_var(Orig, Copy, Bindings) :-
	var(Bindings), !,
	Bindings=[[Orig, Copy]|_].
dup_var(Orig, Copy, [[AnyVar, Copy]|_]) :-
	Orig==AnyVar, !.
dup_var(Orig, Copy, [_|Bindings]) :-
	dup_var(Orig, Copy, Bindings).

% ----- Built-ins ----- %

built_in(true/0, [], []).			% No change
built_in(fail/0, [], []).			% No change
built_in((=)/2, [X, Y], [g, g]) :-
	(atomic(X) ; atomic(Y)). 		% Ground both if either atomic
built_in((=)/2, [X, _Y], [X, X]).		% else bind them
built_in(/('+',2), [X, Y], [X, Y]).		% No change
built_in(/('-',2), [X, Y], [X, Y]).		% No change
built_in(/('*',2), [X, Y], [X, Y]).		% No change
built_in(/('/',2), [X, Y], [X, Y]).		% No change
built_in(/('>=',2), [X, Y], [X, Y]).		% No change
built_in(/('<',2), [X, Y], [X, Y]).		% No change
built_in((is)/2, [_X, Y], [g, Y]).		% Ground result

% ----- Utilities ----- %

worst_case([], _).				%% Doesn't work if any Exits
worst_case([Exit|Exits], Worst) :-		%% fail to match, e.g.
	unify(Exit, Worst),			%% [[s(1)], [f(1)]].
	worst_case(Exits, Worst).

look_up_act(_, Known) :-
	var(Known),
	!, fail.
look_up_act([Functor/Arity, Act, Exit], [[Functor/Arity, KnownAct, Exit]|_]) :-
	equiv(Act, KnownAct).
look_up_act([Functor/Arity, Act, Exit], [_|Known]) :-
	look_up_act([Functor/Arity, Act, Exit], Known).

all_shared(_Act, _Exit) :-			%% Wrong
	fail.       % DD: I have put fail since unify/3 does not exist

/*
all_shared(Act, Exit) :-			%% Wrong
	unify(Act, _, VarModesList),
	bind_all(_, VarModesList),
	unify(Act, Exit, VarModesList).

bind_all(_, VarModesList) :-
	var(VarModesList).
bind_all(Mode, [[Var, Mode]|VarModesList]) :-
	var(Mode),
	bind_all(Mode, VarModesList).
bind_all(Mode, [[_, _]|VarModesList]) :-
	bind_all(Mode, VarModesList).
*/

% Adds Element to the tail of List, an unbound-tail list
add_to_list(Element, List) :-
	var(List),
	List=[Element|_].
add_to_list(Element, [_|List]) :-
	add_to_list(Element, List).

% Membership relation for unbound-tail lists
umember(_, List) :-
	var(List), !, fail.
umember(Element, [Element|_]).
umember(Element, [_|Tail]) :- umember(Element, Tail).

/*
% Membership relation for standard nil-tail lists
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
*/



% Equiv membership relation for standard nil-tail lists
eqmember(X, [Y|_]) :- equiv(X, Y).
eqmember(X, [_|T]) :- eqmember(X, T).

% Pretty prints unbound-tail lists -- dies on NIL tail lists
write_list(List) :-
	dup(List, NewList),
	(var(NewList) -> (name_vars(NewList, 0, _),
			  write(NewList)) ;
		         (write('['),
		      	  write_list2(NewList, 0, _),
		          write('|_].'))), 	% write('].') to write nil tails
	nl.
write_list2([H|T], NextName, NewNextName) :-
	name_vars(H, NextName, TempNextName),
	write(H),
	(nonvar(T) -> (write(','), nl,
		       write(' '),
		       write_list2(T, TempNextName, NewNextName)) ;
		      NewNextName = TempNextName).

name_vars(Term, NextName, NewNextName) :-
	var(Term), !,
	make_name(NextName, Term),
	NewNextName is NextName + 1.
name_vars(Term, NextName, NextName) :-
	atom(Term), !.
name_vars([TermHead|TermTail], NextName, NewNextName) :-
	!, name_vars(TermHead, NextName, TempNextName),
	name_vars(TermTail, TempNextName, NewNextName).
name_vars(Term, NextName, NewNextName) :-
	Term =.. [_|TermArgs],
	name_vars(TermArgs, NextName, NewNextName).

make_name(IntName, Variable) :-
	Count is IntName // 26,
	NewIntName is IntName mod 26 + "A",
	build_name(Count, NewIntName, Name),
	name(Variable, Name).

build_name(0, IntName, [IntName]) :- !.
build_name(Count, IntName, [IntName|Rest]) :- Count>0,
	NewCount is Count - 1,
	build_name(NewCount, IntName, Rest).



% benchmark interface

benchmark(ShowResult) :-
	sdda(ShowResult).

:- include(common).


