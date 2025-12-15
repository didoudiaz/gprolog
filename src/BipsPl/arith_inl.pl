/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : arith_inl.pl                                                    *
 * Descr.: arithmetic (inline) management - defs for meta-call             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2025 Daniel Diaz                                     *
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


:- built_in.

'$use_arith_inl'.


X is Y :-
	X is Y.

X =:= Y :-
	X =:= Y.

X =\= Y :-
	X =\= Y.

X < Y :-
	X < Y.

X =< Y :-
	X =< Y.

X > Y :-
	X > Y.

X >= Y :-
	X >= Y.


'$arith_evaluate'(X, Y) :-		% like X is Y but does not set_bip_name
	'$call_c'('Pl_Arith_Evaluate_2'(X, Y)).




succ(X, Y) :-
	set_bip_name(succ, 2),
	'$call_c_test'('Pl_Succ_2'(X, Y)).




current_evaluable(PI) :-
	set_bip_name(current_evaluable, 1),
	'$current_evaluable'(PI).

'$current_evaluable'(PI) :-
	'$call_c_test'('Pl_Current_Evaluable_1'(PI)).

'$current_evaluable_alt' :-         % used by C code to create a choice-point
	'$call_c_test'('Pl_Current_Evaluable_Alt_0').




evaluable_property(Head, Property) :-
	set_bip_name(evaluable_property, 2),
	callable(Head), !,
	functor(Head, F, N),
	'$evaluable_property1'(F, N, Property).

evaluable_property(Head, Property) :-
	var(Head), !,
	'$current_evaluable'(F/N),
	functor(Head, F, N),
	'$evaluable_property1'(F, N, Property).

evaluable_property(Head, _) :-
	'$pl_err_type'(callable, Head).




'$evaluable_property_pi'(PI, Property) :-
	'$current_evaluable'(PI),
	PI = Func/Arity,
	'$evaluable_property1'(Func, Arity, Property).

'$evaluable_property1'(Func, Arity, Property) :-
	'$check_evaluable_prop'(Property), !,
	'$evaluable_property2'(Property, Func, Arity).




'$check_evaluable_prop'(Property) :-
	var(Property).

'$check_evaluable_prop'(built_in).

'$check_evaluable_prop'(foreign).

'$check_evaluable_prop'(static).

'$check_evaluable_prop'(dynamic).

'$check_evaluable_prop'(iso).

'$check_evaluable_prop'(template(_, _)).

'$check_evaluable_prop'(Property) :-
	'$pl_err_domain'(evaluable_property, Property).




'$evaluable_property2'(built_in, Func, Arity) :-
	'$call_c_test'('Pl_Evaluable_Prop_Built_In_2'(Func, Arity)).

/*
'$evaluable_property2'(foreign, Func, Arity) :-
	'$call_c_test'('Pl_Evaluable_Prop_Foreign_2'(Func, Arity)).
*/

'$evaluable_property2'(static, Func, Arity) :-
	'$call_c_test'('Pl_Evaluable_Prop_Static_2'(Func, Arity)).

/*
'$evaluable_property2'(dynamic, Func, Arity) :-
	'$call_c_test'('Pl_Evaluable_Prop_Dynamic_2'(Func, Arity)).
*/

'$evaluable_property2'(iso, Func, Arity) :-
	'$call_c_test'('Pl_Evaluable_Prop_ISO_2'(Func, Arity)).

'$evaluable_property2'(template(InputTerm, Return), Func, Arity) :-
	'$call_c_test'('Pl_Evaluable_Prop_Signature_3'(Func, Arity, Signature)),
	'$evaluable_prop_template'(Signature, Func, InputTerm, Return).


'$evaluable_prop_template'('=F', Func, InputTerm, Return) :-
	InputTerm = Func,
	Return = float.

'$evaluable_prop_template'('F=F', Func, InputTerm, Return) :-
	InputTerm =.. [Func, float],
	Return = float.

'$evaluable_prop_template'('F=I', Func, InputTerm, Return) :-
	InputTerm =.. [Func, integer],
	Return = float.

'$evaluable_prop_template'('I=I', Func, InputTerm, Return) :-
	InputTerm =.. [Func, integer],
	Return = integer.

'$evaluable_prop_template'('IF=F', Func, InputTerm, Return) :-
	InputTerm =.. [Func, number],
	Return = float.

'$evaluable_prop_template'('IF=IF', Func, InputTerm, Return) :-
	InputTerm =.. [Func, number],
	Return = number.

'$evaluable_prop_template'('I,I=I', Func, InputTerm, Return) :-
	InputTerm =.. [Func, integer, integer],
	Return = integer.

'$evaluable_prop_template'('IF,IF=F', Func, InputTerm, Return) :-
	InputTerm =.. [Func, number, number],
	Return = float.

'$evaluable_prop_template'('IF,IF=IF', Func, InputTerm, Return) :-
	InputTerm =.. [Func, number, number],
	Return = number.

'$evaluable_prop_template'('IF,IF=?', Func, InputTerm, Return) :- % ambiguous
	InputTerm =.. [Func, number, number],
	Return = number.


