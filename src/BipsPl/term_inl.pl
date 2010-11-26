/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : term_inl.pl                                                     * 
 * Descr.: term (inline) management - defs for meta-call                   * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2010 Daniel Diaz                                     * 
 *                                                                         * 
 * GNU Prolog is free software; you can redistribute it and/or modify it   * 
 * under the terms of the GNU Lesser General Public License as published   * 
 * by the Free Software Foundation; either version 3, or any later version.* 
 *                                                                         * 
 * GNU Prolog is distributed in the hope that it will be useful, but       * 
 * WITHOUT ANY WARRANTY; without even the implied warranty of              * 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        * 
 * General Public License for more details.                                * 
 *                                                                         * 
 * You should have received a copy of the GNU Lesser General Public License* 
 * with this program; if not, write to the Free Software Foundation, Inc.  * 
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               * 
 *-------------------------------------------------------------------------*/

/* $Id$ */

:-	built_in.

'$use_term_inl'.


compare(C, T1, T2) :-
	compare(C, T1, T2).


X == Y :-
	X == Y.

X \== Y :-
	X \== Y.

X @< Y :-
	X @< Y.

X @=< Y :-
	X @=< Y.

X @> Y :-
	X @> Y.

X @>= Y :-
	X @>= Y.




arg(N, T, A) :-
	arg(N, T, A).




functor(T, F, N) :-
	functor(T, F, N).




Term =.. List :-
	Term =.. List.


/* these are not inlined but put here for practical reasons */

copy_term(T1, T2) :-
	set_bip_name(copy_term, 2),
	'$call_c_test'('Pl_Copy_Term_2'(T1, T2)).




setarg(ArgNo, Term, NewValue) :-
	set_bip_name(setarg, 3),
	'$call_c_test'('Pl_Setarg_4'(ArgNo, Term, NewValue, true)).


setarg(ArgNo, Term, NewValue, Undo) :-
	set_bip_name(setarg, 4),
	'$call_c_test'('Pl_Setarg_4'(ArgNo, Term, NewValue, Undo)).




term_ref(Term, Ref) :-
	set_bip_name(term_ref, 2),
	'$call_c_test'('Pl_Term_Ref_2'(Term, Ref)).



term_variables(Term, List) :-
	set_bip_name(term_variables, 2),
	'$call_c_test'('Pl_Term_Variables_2'(Term, List)).


term_variables(Term, List, Tail) :-
	set_bip_name(term_variables, 3),
	'$call_c_test'('Pl_Term_Variables_3'(Term, List, Tail)).



subsumes_term(General, Specific) :-
	set_bip_name(subsumes_term, 2),
	'$call_c_test'('Pl_Subsumes_Term_2'(General, Specific)).

	
