/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog buit-in predicates                                       */
/* File  : term_inl.pl                                                     */
/* Descr.: term (inline) management - defs for meta-call                   */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/

:- built_in.

'$use_term_inl'.


compare(C,T1,T2):-
        compare(C,T1,T2).


X == Y:-
        X == Y.

X \== Y:-
        X \== Y.

X @< Y:-
        X @< Y.

X @=< Y:-
        X @=< Y.

X @> Y:-
        X @> Y.

X @>= Y:-
        X @>= Y.




arg(N,T,A):-
        arg(N,T,A).




functor(T,F,N):-
       functor(T,F,N).




Term =.. List:-
        Term =.. List.




copy_term(T1,T2):-
	set_bip_name(copy_term,2),
	'$call_c_test'('Copy_Term_2'(T1,T2)).




setarg(ArgNo,Term,NewValue):-
	set_bip_name(setarg,3),
	'$call_c_test'('Setarg_4'(ArgNo,Term,NewValue,true)).


setarg(ArgNo,Term,NewValue,Undo):-
	set_bip_name(setarg,4),
	'$call_c_test'('Setarg_4'(ArgNo,Term,NewValue,Undo)).




term_ref(Term,Ref):-
	set_bip_name(term_ref,2),
	'$call_c_test'('Term_Ref_2'(Term,Ref)).

