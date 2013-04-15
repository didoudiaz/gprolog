/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : term_inl.pl                                                     *
 * Descr.: term (inline) management - defs for meta-call                   *
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



acyclic_term(X) :-
	set_bip_name(acyclic_term, 1),
	'$call_c_test'('Pl_Acyclic_Term_1'(X)).



term_hash(X, Depth, Range, Hash) :-
	set_bip_name(term_hash, 4),
	'$call_c_test'('Pl_Term_Hash_4'(X, Depth, Range, Hash)).

term_hash(X, Hash) :-
	set_bip_name(term_hash, 2),
	'$call_c_test'('Pl_Term_Hash_2'(X, Hash)).
