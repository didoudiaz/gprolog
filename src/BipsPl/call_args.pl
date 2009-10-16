/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : call_args.pl                                                    * 
 * Descr.: meta call management                                            * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2009 Daniel Diaz                                     * 
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

'$use_call_args'.



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
