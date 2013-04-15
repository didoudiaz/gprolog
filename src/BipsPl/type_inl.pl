/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : type_inl.pl                                                     *
 * Descr.: type testing (inline) management - defs for meta-call           *
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


:-	built_in.

'$use_type_inl'.


var(X) :-
	var(X).

nonvar(X) :-
	nonvar(X).

atom(X) :-
	atom(X).

integer(X) :-
	integer(X).

float(X) :-
	float(X).

number(X) :-
	number(X).

atomic(X) :-
	atomic(X).

compound(X) :-
	compound(X).

callable(X) :-
	callable(X).

ground(X) :-
	ground(X).

is_list(X) :-
	is_list(X).

list(X) :-
	list(X).

partial_list(X) :-
	partial_list(X).

list_or_partial_list(X) :-
	list_or_partial_list(X).


:-	built_in_fd(fd_var / 1, non_fd_var / 1, generic_var / 1, non_generic_var / 1).

fd_var(X) :-
	fd_var(X).

non_fd_var(X) :-
	non_fd_var(X).

generic_var(X) :-
	generic_var(X).

non_generic_var(X) :-
	non_generic_var(X).
