/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : throw.pl                                                        *
 * Descr.: exception management (throw)                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2014 Daniel Diaz                                     *
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
/*
'$throw'(Ball, CallerModule, CallerFunc, CallerArity) :-
	format('  throw(~w,~w,~w,~w)~n',[Ball, CallerModule, CallerFunc, CallerArity]),
	fail.
*/

'$throw'(Ball, _, _, _) :-
	nonvar(Ball), !,
	'$sys_var_put'(8, Ball),
	'$unwind'(Ball).

'$throw'(_Ball, _CallerModule, CallerFunc, CallerArity) :-
%	'$call_c'('Pl_Set_Error_MFA'(CallerModule, CallerFunc, CallerArity)), % FIXME
	'$call_c'('Pl_Set_Bip_Name_2'(CallerFunc, CallerArity)),
	'$pl_err_instantiation'.




'$unwind'(Ball) :-
	'$sys_var_read'(7, Handler),
	'$call_c'('Pl_Throw_2'(Ball, Handler)),              % mainly does a cut
	fail.


% FIXME remove this when MFA is correctly handled everywhere (in call/catch)

'$throw_internal'(Ball, CallerModule, CallInfo) :- %NB not really CallerModule but Module (see call.pl comment)
	'$call_c'('Pl_Call_Info_Bip_Name_1'(CallInfo)),
	'$call_c'('Pl_Get_Bip_Name_2'(CallerFunc, CallerArity)),
%format('In throw(~w,~w,~w,~w)~n',[Ball, CallerModule, CallerFunc, CallerArity]),
	'$throw'(Ball, CallerModule, CallerFunc, CallerArity).
	

