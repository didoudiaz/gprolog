/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : FD constraint solver buit-in predicates                         * 
 * File  : fd_symbolic.pl                                                  * 
 * Descr.: symbolic constraints management                                 * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2001 Daniel Diaz                                     * 
 *                                                                         * 
 * GNU Prolog is free software; you can redistribute it and/or modify it   * 
 * under the terms of the GNU General Public License as published by the   * 
 * Free Software Foundation; either version 2, or any later version.       * 
 *                                                                         * 
 * GNU Prolog is distributed in the hope that it will be useful, but       * 
 * WITHOUT ANY WARRANTY; without even the implied warranty of              * 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        * 
 * General Public License for more details.                                * 
 *                                                                         * 
 * You should have received a copy of the GNU General Public License along * 
 * with this program; if not, write to the Free Software Foundation, Inc.  * 
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     * 
 *-------------------------------------------------------------------------*/

/* $Id$ */

:-	built_in_fd.

'$use_fd_symbolic'.


fd_all_different(L) :-
	set_bip_name(fd_all_different, 1),
	'$call_c_test'('Fd_All_Different_1'(L, L)).



fd_element(I, List, V) :-
	set_bip_name(fd_element, 3),
	'$fd_element'(I, List, V).


'$fd_element'(I, List, V) :-
	fd_tell(fd_element(I, List, V)).

	


fd_element_var(I, List, V) :-
	set_bip_name(fd_element_var, 3),
	fd_tell(fd_element_var(I, List, V)).




fd_atmost(N, List, V) :-
	set_bip_name(fd_atmost, 3),
	fd_tell(fd_atmost(N, List, V)).




fd_atleast(N, List, V) :-
	set_bip_name(fd_atleast, 3),
	fd_tell(fd_atleast(N, List, V)).




fd_exactly(N, List, V) :-
	set_bip_name(fd_exactly, 3),
	fd_tell(fd_exactly(N, List, V)).




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
