/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint definition file to C code compiler                *
 * File  : read_file.pl                                                    *
 * Descr.: source file reading                                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2013 Daniel Diaz                                     *
 *                                                                         *
 * This file is part of GNU Prolog                                         *
 *                                                                         *
 * GNU Prolog is free software: you can redistribute it and/or             *
 * modify it under the terms of either:                                    *
 *                                                                         *
 *   - the GNU Lesser General Public License as published by the Free      *
 *     Software Foundation; either version 3 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or                                                                      *
 *                                                                         *
 *   - the GNU General Public License as published by the Free             *
 *     Software Foundation; either version 2 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or both in parallel, as here.                                           *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful,           *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received copies of the GNU General Public License and   *
 * the GNU Lesser General Public License along with this program.  If      *
 * not, see http://www.gnu.org/licenses/.                                  *
 *-------------------------------------------------------------------------*/


file_to_token_lst(LToken) :-
	get_code(stream_fd, C),
	file_to_token_lst1(C, LToken).


file_to_token_lst1(-1, []) :-
	!.

file_to_token_lst1(C, LToken) :-
	C =< 32, !,
	get_code(stream_fd, C1),
	file_to_token_lst1(C1, LToken).

file_to_token_lst1(47, LToken) :-
	peek_code(stream_fd, 42), !,
	get_code(stream_fd, _),
	skip_until(42, 47, f),
	get_code(stream_fd, C1),
	file_to_token_lst1(C1, LToken).

file_to_token_lst1(37, LToken) :-
	peek_code(stream_fd, 123), !,
	get_code(stream_fd, _),
	stream_line_column(stream_fd, Line1, _),
	format(stream_c, '~n/* line:~d begin included code */~n', [Line1]),
	skip_until(37, 125, t),
	stream_line_column(stream_fd, Line2, _),
	format(stream_c, '~n/* line:~d end   included code */~n', [Line2]),
	get_code(stream_fd, C1),
	file_to_token_lst1(C1, LToken).

file_to_token_lst1(C, [t(Token, Line, Col1)|LToken]) :-
	stream_line_column(stream_fd, Line, Col),
	Col1 is Col - 1,
	one_token(C, Token, C1),
	file_to_token_lst1(C1, LToken).



skip_until(C1, C2, Echo) :-
	g_assign(next, C1),
	repeat,
	g_read(next, Next),
	get_code(stream_fd, C),
	skip1(C, Next, C1, C2, Echo), !.




skip1(-1, _, C1, C2, _) :-
	error('EOF reached before ~c~c found', [C1, C2]).

skip1(Next, Next, _, Next, _) :-
	!.

skip1(Next, Next, Next, C2, _) :-
	!,
	g_assign(next, C2),
	fail.

skip1(C, Next, C1, Next, Echo) :-
	!,
	(   Echo = t ->
	    put_code(stream_c, C1),
	    put_code(stream_c, C)
	;   true
	),
	g_assign(next, C1),
	fail.

skip1(C, _, _, _, Echo) :-
	(   Echo = t ->
	    put_code(stream_c, C)
	;   true
	),
	fail.




one_token(C, Token, C2) :-
	(   C >= 97,
	    C =< 122,
	    C1 = C
	;   C >= 65,
	    C =< 90,
	    C1 is C + 97 - 65
	;   C = 39,
	    C1 = C
	), !,
	unget_code(stream_fd, C1),
	read_atom(stream_fd, Token1),
	(   C >= 65,
	    C =< 90 ->
	    sub_atom(Token1, 1, _, 0, A),
	    char_code(AC, C),
	    atom_concat(AC, A, Token2)
	;   Token2 = Token1
	),
	(   keyword(Token2) ->
	    Token = Token2
	;   Token = ident(Token2)
	),
	get_code(stream_fd, C2).

one_token(C, Token, C1) :-
	C >= 48,
	C =< 57, !,
	unget_code(stream_fd, C),
	read_integer(stream_fd, Token),
	get_code(stream_fd, C1).

one_token(C, Token, C2) :-
	get_code(stream_fd, C1),
	char_code(A1, C),
	char_code(A2, C1),
	atom_concat(A1, A2, A),
	(   member(A, [/<, />, ==, '!=', <=, >=, &&, '||', .., ++, --, **, //, '%%', '|<', '|>']) ->
	    get_code(stream_fd, C2),
	    Token = A
	;   Token = A1,
	    C2 = C1
	).


keyword(min).
keyword(max).
keyword(dom).
keyword(val).

keyword(int).
keyword(range).
keyword(fdv).
keyword(any).
keyword(l_int).
keyword(l_range).
keyword(l_fdv).
keyword(l_any).

keyword(when).
keyword(wait_switch).
keyword(case).
keyword(start).
keyword(trigger).
keyword(also).
keyword(on).
keyword(always).
keyword(fail).
keyword(exit).
keyword(if).
keyword(stop).

keyword(forall).
keyword(of).
keyword(do).

keyword(in).
keyword(max_integer).
