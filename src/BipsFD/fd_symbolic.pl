/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_symbolic.pl                                                  *
 * Descr.: symbolic constraints management                                 *
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


:-	built_in_fd.

'$use_fd_symbolic'.


fd_all_different(L) :-
	set_bip_name(fd_all_different, 1),
	'$call_c_test'('Pl_Fd_All_Different_1'(L, L)).



fd_element(I, List, V) :-
	set_bip_name(fd_element, 3),
	'$fd_element'(I, List, V).


'$fd_element'(I, List, V) :-
	fd_tell(pl_fd_element(I, List, V)).




fd_element_var(I, List, V) :-
	set_bip_name(fd_element_var, 3),
	fd_tell(pl_fd_element_var(I, List, V)).




fd_atmost(N, List, V) :-
	set_bip_name(fd_atmost, 3),
	fd_tell(pl_fd_atmost(N, List, V)).




fd_atleast(N, List, V) :-
	set_bip_name(fd_atleast, 3),
	fd_tell(pl_fd_atleast(N, List, V)).




fd_exactly(N, List, V) :-
	set_bip_name(fd_exactly, 3),
	fd_tell(pl_fd_exactly(N, List, V)).




fd_relation(Tuples, Vars) :-
	set_bip_name(fd_relation, 2),
	'$check_list'(Tuples),
	'$check_list_or_partial_list'(Vars),
	'$lines_to_columns'(Tuples, CTuples),
	'$fd_relationc1'(CTuples, Vars, _).



fd_relationc(CTuples, Vars) :-
	set_bip_name(fd_relationc, 2),
	'$check_list'(CTuples),
	'$check_list_or_partial_list'(Vars),
	'$fd_relationc1'(CTuples, Vars, _).




'$fd_relationc1'([], [], _).

'$fd_relationc1'([C|CTuples], [X|Vars], R) :-
	'$fd_element'(R, C, X),
	'$fd_relationc1'(CTuples, Vars, R).




'$lines_to_columns'([[]|_], []) :-
	!.

'$lines_to_columns'(Tuples, [Column|Columns]) :-
	'$create_column'(Tuples, Column, Tuples1),
	'$lines_to_columns'(Tuples1, Columns).




'$create_column'([], [], []).

'$create_column'([[X|L]|Tuples], [X|Column], [L|Tuples1]) :-
	'$create_column'(Tuples, Column, Tuples1).
