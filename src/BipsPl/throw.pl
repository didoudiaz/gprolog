/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : throw.pl                                                        *
 * Descr.: exception management (throw)                                    *
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

'$use_throw'.


'$throw'(Ball, Func, Arity, DebugCall) :-
	'$call_c'('Pl_Save_Call_Info_3'(Func, Arity, DebugCall)),
	'$throw1'(Ball, 0).


'$throw1'(Ball, CallInfo) :-
	'$call_c'('Pl_Load_Call_Info_Arg_1'(1)),   % to ensure CallInfo is deref
	'$throw_internal'(Ball, CallInfo).


'$throw_internal'(Ball, CallInfo) :-
	(   var(Ball) ->
	    '$call_c'('Pl_Call_Info_Bip_Name_1'(CallInfo)),
	    '$pl_err_instantiation'
	;   true
	),
	'$sys_var_put'(8, Ball),
	'$unwind'(Ball).




'$unwind'(Ball) :-
	'$sys_var_read'(7, Handler),
	'$call_c'('Pl_Throw_2'(Ball, Handler)),              % mainly does a cut
	fail.
