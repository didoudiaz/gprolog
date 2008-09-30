/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : type_inl.pl                                                     * 
 * Descr.: type testing (inline) management - defs for meta-call           * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2008 Daniel Diaz                                     * 
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
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               * 
 *-------------------------------------------------------------------------*/

/* $Id$ */

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
