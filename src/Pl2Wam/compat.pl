
:-	op(0, fx, dynamic).
:-	op(0, fx, discontiguous).
:-	op(0, fx, multifile).

prolog_file_name(PlFile, PlFile1) :-
	decompose_file_name(PlFile, _Dir, _Prefix, Suffix),
	(   (   PlFile = user
	    ;   Suffix \== ''
	    ) ->
	    PlFile1 = PlFile
	;   atom_concat(PlFile, '.pl', PlFile1)
	).


last_read_start_line_column(Line, Col) :-
	g_read('$last_line', Line),
	g_read('$last_col', Col).

stream_line_column(Stream, Line, Col) :-
	line_count(Stream, Count),
	line_position(Stream, Pos),
	Line is Count + 1,
	Col is Pos + 1,
	g_assign('$last_line', Line),
	g_assign('$last_col', Col).



date_time(dt(0, 0, 0, 0, 0, 0)).


numbervars(T) :-
	numbervars(T, 0, _).




decompose_file_name(Path, Dir, Prefix, Suffix) :-
	atom_length(Path, L),
	Before is L - 1,
	find_dir_and_file_name(Path, Before, Dir, FileName),
	(   sub_atom(FileName, LgPrefix, 1, _, '.') ->
	    sub_atom(FileName, 0, LgPrefix, LgSuffix, Prefix),
	    sub_atom(FileName, LgPrefix, LgSuffix, 0, Suffix)
	;   Prefix = FileName,
	    Suffix = ''
	), !.


find_dir_and_file_name(Path, Before, '', Path) :-
	Before < 0, !.

find_dir_and_file_name(Path, Before, Dir, FileName) :-
	sub_atom(Path, Before, 1, After, /),
	Before1 is Before + 1,
	sub_atom(Path, 0, Before1, _, Dir),
	sub_atom(Path, _, After, 0, FileName), !.

find_dir_and_file_name(Path, Before, Dir, FileName) :-
	Before1 is Before - 1,
	find_dir_and_file_name(Path, Before1, Dir, FileName).




number_atom(N, A) :-
	(   number(N) ->
	    number_chars(N, LCode),
	    atom_chars(A, LCode)
	;   atom_chars(A, LCode),
	    number_chars(N, LCode)
	).

'$catch'(Goal, Catcher, Recovery, _, _, _) :-
	catch(Goal, Catcher, Recovery).



'$aux_name'(Name) :-
	sub_atom(Name, _, 5, _, '_$aux'), !.



'$make_aux_name'(Pred, N, Aux, AuxName) :-
	(   sub_atom(Pred, LgBefore, 5, _, '_$aux') ->
	    sub_atom(Pred, 0, LgBefore, _, Pred1)
	;   number_atom(N, AN),
	    atom_concat($, Pred, Pred2),
	    atom_concat(Pred2, /, Pred3),
	    atom_concat(Pred3, AN, Pred1)
	),
	number_atom(Aux, ANo),
	atom_concat('_$aux', ANo, AAux),
	atom_concat(Pred1, AAux, AuxName).




'$pred_without_aux'(Func, Arity, Func1, Arity1) :-
	(   sub_atom(Func, LgBefore, 5, _, '_$aux') ->
	    sub_atom(Func, B, 1, _, /),
	    L is B - 1,
	    sub_atom(Func, 1, L, _, Func1),
	    B1 is B + 1,
	    A is LgBefore - B - 1,
	    sub_atom(Func, B1, A, _, SA1),
	    number_atom(Arity1, SA1), !
	;   Func1 = Func,
	    Arity1 = Arity
	).
