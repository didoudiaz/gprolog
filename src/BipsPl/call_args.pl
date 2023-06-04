/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : call_args.pl                                                    *
 * Descr.: meta call management                                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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

'$use_call_args'.

:- meta_predicate(call_with_args(1)).
:- meta_predicate(call_with_args(1, ?)).
:- meta_predicate(call_with_args(2, ?, ?)).
:- meta_predicate(call_with_args(3, ?, ?, ?)).
:- meta_predicate(call_with_args(4, ?, ?, ?, ?)).
:- meta_predicate(call_with_args(5, ?, ?, ?, ?, ?)).
:- meta_predicate(call_with_args(6, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(call_with_args(7, ?, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(call_with_args(8, ?, ?, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(call_with_args(9, ?, ?, ?, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(call_with_args(10, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)).

call_with_args(_F) :-
	'$call_c'('Pl_Call_Closure'(call_with_args, 0), [jump, by_value]).

call_with_args(_F, _A1) :-
	'$call_c'('Pl_Call_Closure'(call_with_args, 1), [jump, by_value]).

call_with_args(_F, _A1, _A2) :-
	'$call_c'('Pl_Call_Closure'(call_with_args, 2), [jump, by_value]).

call_with_args(_F, _A1, _A2, _A3) :-
	'$call_c'('Pl_Call_Closure'(call_with_args, 3), [jump, by_value]).

call_with_args(_F, _A1, _A2, _A3, _A4) :-
	'$call_c'('Pl_Call_Closure'(call_with_args, 4), [jump, by_value]).

call_with_args(_F, _A1, _A2, _A3, _A4, _A5) :-
	'$call_c'('Pl_Call_Closure'(call_with_args, 5), [jump, by_value]).

call_with_args(_F, _A1, _A2, _A3, _A4, _A5, _A6) :-
	'$call_c'('Pl_Call_Closure'(call_with_args, 6), [jump, by_value]).

call_with_args(_F, _A1, _A2, _A3, _A4, _A5, _A6, _A7) :-
	'$call_c'('Pl_Call_Closure'(call_with_args, 7), [jump, by_value]).

call_with_args(_F, _A1, _A2, _A3, _A4, _A5, _A6, _A7, _A8) :-
	'$call_c'('Pl_Call_Closure'(call_with_args, 8), [jump, by_value]).

call_with_args(_F, _A1, _A2, _A3, _A4, _A5, _A6, _A7, _A8, _A9) :-
	'$call_c'('Pl_Call_Closure'(call_with_args, 9), [jump, by_value]).

call_with_args(_F, _A1, _A2, _A3, _A4, _A5, _A6, _A7, _A8, _A9, _A10) :-
	'$call_c'('Pl_Call_Closure'(call_with_args, 10), [jump, by_value]).



:- meta_predicate(call(1, ?)).
:- meta_predicate(call(2, ?, ?)).
:- meta_predicate(call(3, ?, ?, ?)).
:- meta_predicate(call(4, ?, ?, ?, ?)).
:- meta_predicate(call(5, ?, ?, ?, ?, ?)).
:- meta_predicate(call(6, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(call(7, ?, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(call(8, ?, ?, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(call(9, ?, ?, ?, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(call(10, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)).

call(_Closure, _A1) :-
	'$call_c'('Pl_Call_Closure'(call, 1), [jump, by_value]).

call(_Closure, _A1, _A2) :-
	'$call_c'('Pl_Call_Closure'(call, 2), [jump, by_value]).

call(_Closure, _A1, _A2, _A3) :-
	'$call_c'('Pl_Call_Closure'(call, 3), [jump, by_value]).

call(_Closure, _A1, _A2, _A3, _A4) :-
	'$call_c'('Pl_Call_Closure'(call, 4), [jump, by_value]).

call(_Closure, _A1, _A2, _A3, _A4, _A5) :-
	'$call_c'('Pl_Call_Closure'(call, 5), [jump, by_value]).

call(_Closure, _A1, _A2, _A3, _A4, _A5, _A6) :-
	'$call_c'('Pl_Call_Closure'(call, 6), [jump, by_value]).

call(_Closure, _A1, _A2, _A3, _A4, _A5, _A6, _A7) :-
	'$call_c'('Pl_Call_Closure'(call, 7), [jump, by_value]).

call(_Closure, _A1, _A2, _A3, _A4, _A5, _A6, _A7, _A8) :-
	'$call_c'('Pl_Call_Closure'(call, 8), [jump, by_value]).

call(_Closure, _A1, _A2, _A3, _A4, _A5, _A6, _A7, _A8, _A9) :-
	'$call_c'('Pl_Call_Closure'(call, 9), [jump, by_value]).

call(_Closure, _A1, _A2, _A3, _A4, _A5, _A6, _A7, _A8, _A9, _A10) :-
	'$call_c'('Pl_Call_Closure'(call, 10), [jump, by_value]).
