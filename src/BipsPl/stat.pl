/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog buit-in predicates                                       */
/* File  : stat.pl                                                         */
/* Descr.: statistics predicate management                                 */
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

:-	built_in.

'$use_stat'.


statistics :-
	set_bip_name(statistics, 0),
	'$call_c'('Statistics_0').


statistics(Key, Values) :-
	set_bip_name(statistics, 2),
	'$check_stat_key'(Key), !,
	(   Values=[Val1, Val2] ->
	    true
	;   '$pl_err_domain'(statistics_value, Values)
	),
	'$stat'(Key, Val1, Val2).




'$check_stat_key'(Key) :-
	var(Key).

'$check_stat_key'(user_time).
'$check_stat_key'(runtime).

'$check_stat_key'(system_time).

'$check_stat_key'(cpu_time).

'$check_stat_key'(real_time).

'$check_stat_key'(local_stack).

'$check_stat_key'(global_stack).

'$check_stat_key'(trail_stack).

'$check_stat_key'(cstr_stack).

'$check_stat_key'(Key) :-
	'$pl_err_domain'(statistics_key, Key).




'$stat'(X, SinceStart, SinceLast) :-
	(   atom(X) ->
	    (   X=user_time
	    ;   X=runtime
	    ), !
	;   X=user_time
	),
	'$call_c_test'('Statistics_User_Time_2'(SinceStart, SinceLast)).

'$stat'(system_time, SinceStart, SinceLast) :-
	'$call_c_test'('Statistics_System_Time_2'(SinceStart, SinceLast)).

'$stat'(cpu_time, SinceStart, SinceLast) :-
	'$call_c_test'('Statistics_Cpu_Time_2'(SinceStart, SinceLast)).

'$stat'(real_time, SinceStart, SinceLast) :-
	'$call_c_test'('Statistics_Real_Time_2'(SinceStart, SinceLast)).

'$stat'(local_stack, Used, Free) :-
	'$call_c_test'('Statistics_Local_Stack_2'(Used, Free)).

'$stat'(global_stack, Used, Free) :-
	'$call_c_test'('Statistics_Global_Stack_2'(Used, Free)).

'$stat'(trail_stack, Used, Free) :-
	'$call_c_test'('Statistics_Trail_Stack_2'(Used, Free)).

'$stat'(cstr_stack, Used, Free) :-
	'$call_c_test'('Statistics_Cstr_Stack_2'(Used, Free)).




user_time(SinceStart) :-
	set_bip_name(user_time, 1),
	'$call_c_test'('User_Time_1'(SinceStart)).




system_time(SinceStart) :-
	set_bip_name(system_time, 1),
	'$call_c_test'('System_Time_1'(SinceStart)).




cpu_time(SinceStart) :-
	set_bip_name(cpu_time, 1),
	'$call_c_test'('Cpu_Time_1'(SinceStart)).




real_time(SinceStart) :-
	set_bip_name(real_time, 1),
	'$call_c_test'('Real_Time_1'(SinceStart)).
