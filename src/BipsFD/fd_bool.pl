/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_bool.pl                                                      *
 * Descr.: boolean and Meta-constraint predicate management                *
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

'$use_fd_bool'.


'$truth_of'(Cstr, B) :-
	'$call_c_test'('Pl_Fd_Bool_Meta_3'(Cstr, B, 1)).




#\ LE :-
	set_bip_name(#\, 1),
	'$call_c_test'('Pl_Fd_Bool_Meta_3'(LE, 0, 0)).

LE #<=> RE :-
	set_bip_name(#<=>, 2),
	'$call_c_test'('Pl_Fd_Bool_Meta_3'(LE, RE, 1)).

LE #\<=> RE :-
	set_bip_name(#\<=>, 2),
	'$call_c_test'('Pl_Fd_Bool_Meta_3'(LE, RE, 2)).

LE ## RE :-
	set_bip_name(#, 2),
	'$call_c_test'('Pl_Fd_Bool_Meta_3'(LE, RE, 2)).

LE #==> RE :-
	set_bip_name(#==>, 2),
	'$call_c_test'('Pl_Fd_Bool_Meta_3'(LE, RE, 3)).

LE #\==> RE :-
	set_bip_name(#\==>, 2),
	'$call_c_test'('Pl_Fd_Bool_Meta_3'(LE, RE, 4)).

LE #/\ RE :-
	set_bip_name(#/\, 2),
	'$call_c_test'('Pl_Fd_Bool_Meta_3'(LE, RE, 5)).

LE #\/\ RE :-
	set_bip_name(#\/\, 2),
	'$call_c_test'('Pl_Fd_Bool_Meta_3'(LE, RE, 6)).

LE #\/ RE :-
	set_bip_name(#\/, 2),
	'$call_c_test'('Pl_Fd_Bool_Meta_3'(LE, RE, 7)).

LE #\\/ RE :-
	set_bip_name(#\\/, 2),
	'$call_c_test'('Pl_Fd_Bool_Meta_3'(LE, RE, 8)).



	% Reified interval

fd_reified_in(X, L, U, B) :-
	set_bip_name(fd_reified_in, 4),
	'$call_c_test'('Pl_Fd_Reified_In'(X, L, U, B)).





        % Symbolic boolean constraints


fd_cardinality(List, Count) :-
	fd_max_integer(Inf),
	set_bip_name(fd_cardinality, 2),
	'$fd_domain'(Count, 0, Inf),                 % to check type of Count
	'$fd_cardinality'(List, Count).

'$fd_cardinality'(List, Count) :-
	'$check_list'(List),
	'$fd_cardinality1'(List, Count).


'$fd_cardinality1'([], 0).

'$fd_cardinality1'([C|List], Count1) :-
	'$fd_cardinality1'(List, Count),
	'$truth_of'(C, B),
	Count1 #= Count + B.




fd_cardinality(L, List, U) :-
	set_bip_name(fd_cardinality, 3),
	'$fd_domain'(Count, L, U),
	'$fd_cardinality'(List, Count).


fd_at_least_one(List) :-
	set_bip_name(fd_at_least_one, 1),
	'$fd_cardinality'(List, Count),
	Count #>= 1 .




fd_at_most_one(List) :-
	set_bip_name(fd_at_most_one, 1),
	'$fd_cardinality'(List, Count),
	Count #=< 1 .




fd_only_one(List) :-
	set_bip_name(fd_only_one, 1),
	'$fd_cardinality'(List, 1).
