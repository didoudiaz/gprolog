/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : catch.pl                                                        * 
 * Descr.: exception management (catch)                                    * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2001 Daniel Diaz                                     * 
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
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     * 
 *-------------------------------------------------------------------------*/

/* $Id$ */

:-	built_in.

'$use_catch'.



          /* Warning the name '$catch_internal1' is tested by the debugger */

'$catch'(Goal, Catch, Recovery, Func, Arity, DebugCall) :-
	'$call_c'('Save_Call_Info_3'(Func, Arity, DebugCall)),
	'$catch1'(Goal, Catch, Recovery, 0).
	
'$catch1'(Goal, Catch, Recovery, CallInfo) :-
	'$call_c'('Load_Call_Info_Arg_1'(3)),   % to ensure CallInfo is deref
	'$catch_internal'(Goal, Catch, Recovery, CallInfo).


'$catch_internal'(Goal, Catch, Recovery, CallInfo) :-
	'$sys_var_read'(7, Handler),
	'$sys_var_put'(8, '$no_ball$'),
	'$catch_internal1'(Goal, Catch, Recovery, CallInfo, Handler).


'$catch_internal1'(Goal, _Catch, _Recovery, CallInfo, Handler) :-
	'$get_current_B'(B),
	'$sys_var_write'(7, B),
% for debug
% format('~N*** ~d for catch(~w,~w,~w)~n',[B,Goal,_Catch,_Recovery]),
	'$call_internal'(Goal, CallInfo),
	'$get_current_B'(B1),
	(   B1 > B ->
	    '$trail_handler'(B)
	;   !
	),
	'$sys_var_write'(7, Handler).

% for debug
%'$catch_internal1'(_,_,_,_,_):-
%        '$get_current_B'(B),
%        '$sys_var_get'(8,Ball),
%	format('~N*** ~d catching throw(~w)~n',[B,Ball]),
%        fail.

'$catch_internal1'(_, Catch, Recovery, CallInfo, Handler) :-
                                                        % after throw or fail
	'$sys_var_write'(7, Handler),
	'$sys_var_get'(8, Ball),
	Ball \== '$no_ball$',
	'$catch_a_throw'(Ball, Catch, Recovery, CallInfo, Handler).




'$catch_a_throw'(Ball, _, _, _, Handler) :-                       % for abort
	nonvar(Ball),
	Ball = '$catch_sync'(B), !,
	(   Handler > B ->
	    '$unwind'('$catch_sync'(B))
	;   '$catch_fail_now'(B)
	).

'$catch_a_throw'(Ball, Ball, Recovery, CallInfo, _) :-
                                                     % normal throw - unifies
	!,
	'$sys_var_put'(8, '$no_ball$'),
	'$call_internal'(Recovery, CallInfo).

'$catch_a_throw'(Ball, _, _, _, _) :-         % normal throw - does not unify
	'$unwind'(Ball).




'$trail_handler'(_).

'$trail_handler'(Handler) :-
	'$sys_var_write'(7, Handler),
	fail.




'$catch_sync_for_fail_at'(B) :-
	'$sys_var_read'(7, Handler),
	(   Handler > B ->
	    throw('$catch_sync'(B))
	;   '$catch_fail_now'(B)
	).




'$catch_fail_now'(B) :-
	'$set_current_B'(B),
	fail.
