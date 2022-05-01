	/* Array procedures */

	/*---------------------------------------------------------*
	 * An array NL x NC elements is represented as follows :   *
	 * A = [L_1, ..., L_NL] with L_i = [X_i_1, ..., X_i_NC]    *
	 * Hence :                                                 *
	 * A = [ [X_1_1,..., X_1_NC], ..., [X_NL_1,..., X_NL_NC] ] *
	 *---------------------------------------------------------*/

	% create_array(+NL, +NC, ?A): creates an array (with unbound variables)
	% NL: nb of lines   NC:nb of columns   A:array

create_array(NL, NC, A) :-
	create_array1(0, NL, NC, A), !.


create_array1(NL, NL, _, []).

create_array1(I, NL, NC, [L|A]) :-
	create_one_line(0, NC, L),
	I1 is I + 1,
	create_array1(I1, NL, NC, A).




create_one_line(NC, NC, []).

create_one_line(J, NC, [_|L]) :-
	J1 is J + 1,
	create_one_line(J1, NC, L).




	% array_elem(+A, +I, +J, ?X): returns an element
	% A:array   I: line no   J: column no  X: the element

array_elem(A, I, J, X) :-
	nth1(I, A, L),
	nth1(J, L, X).



	% array_values(+A, ?Values): returns all elements
	%     A: array   Values: list of elements

array_values([], []).

array_values([L|A], Values) :-
	array_values(A, V),
	append(L, V, Values).




	% array_line(+A, +I, ?C): returns the Ith line
	% A:array   I: line no   L: the line

array_line(A, I, L) :-
	nth1(I, A, L).




	% array_column(+A, +J, ?C): returns the Jth column
	% A:array   J: column no   C: the column

array_column([], _, []).

array_column([L|A], J, [X|C]) :-
	nth1(J, L, X),
	array_column(A, J, C).

	
	


	% for_each_line(+A, +P): invokes a user procedure for each line
	% A:array   P: program term
	% calls: array_prog(P, L) for each line L (L is a list)

for_each_line([], _).

for_each_line([L|A], P) :-
	array_prog(P, L),
	for_each_line(A, P).




	% for_each_column(+A, +P): invokes a user procedure for each column
	% A:array   P: program term
	% calls: array_prog(P, L) for each column L (L is a list)

for_each_column([[]|_], _) :-
	!.

for_each_column(A, P) :-
	create_column(A, C, A1),
	array_prog(P, C),
	for_each_column(A1, P).




create_column([], [], []).

create_column([[X|L]|A], [X|C], [L|A1]) :-
	create_column(A, C, A1).




	% for_each_diagonal(+A, +NL, +NC, +P): invokes a user procedure for each diagonal
	% A:array   NL: nb of lines
	% NC:nb of columns   P: program term
	% calls: array_prog(P, D) for each diagonal D (D is a list)

for_each_diagonal(A, NL, NC, P) :-
	NbDiag is 2 * (NL + NC - 1),            % numbered from 0 to NbDiag-1
	create_lst_diagonal(0, NbDiag, LD),
	fill_lst_diagonal(A, 0, NL, NC, LD, LD1), !,
	for_each_line(LD1, P).




create_lst_diagonal(NbDiag, NbDiag, []).

create_lst_diagonal(I, NbDiag, [[]|LD]) :-
	I1 is I + 1,
	create_lst_diagonal(I1, NbDiag, LD).




fill_lst_diagonal([], _, _, _, LD, LD).

fill_lst_diagonal([L|A], I, NL, NC, LD, LD2) :-
	I1 is I + 1,
	fill_lst_diagonal(A, I1, NL, NC, LD, LD1),
	one_list(L, I, NL, 0, NC, LD1, LD2).





one_list([], _, _, _, _, LD, LD).

one_list([X|L], I, NL, J, NC, LD, LD3) :-
	J1 is J + 1,
	one_list(L, I, NL, J1, NC, LD, LD1),
	NoDiag1 is I + J,
	NoDiag2 is I + NC - J + NL + NC - 2,
	add_in_lst_diagonal(0, NoDiag1, X, LD1, LD2),
	add_in_lst_diagonal(0, NoDiag2, X, LD2, LD3).





add_in_lst_diagonal(NoDiag, NoDiag, X, [D|LD], [[X|D]|LD]).

add_in_lst_diagonal(K, NoDiag, X, [D|LD], [D|LD1]) :-
	K1 is K + 1,
	add_in_lst_diagonal(K1, NoDiag, X, LD, LD1).






	% for_each_big_diagonal(+A, +N, +P): invokes a user procedure for each major diagonal
	% A:array   N: nb of lines/columns (must be a square)
	% P: program term
	% calls: array_prog(P, D) for each diagonal D (D is a list)


for_each_big_diagonal(A, N, P) :-
	big_diags(A, 0, N, D1, D2),
	array_prog(P, D1),
	array_prog(P, D2).


big_diags([], _, _, [], []).

big_diags([L|A], I, J, [X|D1], [Y|D2]) :-
	I1 is I + 1,
	J1 is J - 1,
	nth1(I1, L, X),
	nth1(J, L, Y),
	big_diags(A, I1, J1, D1, D2).




	% write_array(+A, +Format, +Sep): writes an array
	% A:array   Format: format for element writing
        % Sep: nb of spaces between 2 elements of a line

write_array([], _, _).

write_array([L|A], Format, Sep) :-
	write_array_line(L, Format, Sep),
	nl,
	write_array(A, Format, Sep).


write_array_line([], _, _).

write_array_line([X|L], Format, Sep) :-
	format(Format, [X]),
	tab(Sep),
	write_array_line(L, Format, Sep).


	% array_labeling(+A): call fd_labeling line by line
	% A:array


array_labeling([]).

array_labeling([L|A]) :-
	fd_labeling(L),
	array_labeling(A).
