/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : debugger.pl                                                     *
 * Descr.: debugger                                                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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

'$init_debugger' :-                           % called by Debug_Initializer()
	'$sys_var_write'(13, 1),                        % debugger is present
	DebugInfo = d(0, []),
	g_link('$debug_info', DebugInfo),
	'$debug_switch_off'.




'$debug_switch_off' :-
	g_assign('$debug_mode', nodebug),
	g_assign('$debug_next', nodebug),
	g_assign('$debug_leash', 31),
	g_assign('$debug_depth', 10),
	'$call_c'('Pl_Reset_Debug_Call_Code_0').



'$debug_switch_on'(DebugMode) :-
	g_assign('$debug_mode', DebugMode),
	g_assign('$debug_next', DebugMode),
	g_read('$debug_info', DebugInfo),
	setarg(1, DebugInfo, 0, false),
	setarg(2, DebugInfo, [], false),
	'$call_c'('Pl_Set_Debug_Call_Code_0').




           % Debugger built-in predicates

wam_debug :-
	set_bip_name(wam_debug, 0),
	'$call_c'('Pl_Debug_Wam').




notrace :-
	set_bip_name(notrace, 0),
	nodebug.




nodebug :-
	set_bip_name(nodebug, 0),
	'$debug_switch_off',
	'$show_debugger_mode'.


trace :-
	set_bip_name(trace, 0),
	'$debug_switch_on'(trace),
	'$show_debugger_mode'.


debug :-
	set_bip_name(debug, 0),
	'$debug_switch_on'(debug),
	'$show_debugger_mode'.




'$show_debugger_mode' :-
	g_read('$debug_mode', DebugMode),
	'$show_debugger_mode1'(DebugMode),
	nl(debugger_output).


'$show_debugger_mode1'(nodebug) :-
	write(debugger_output, 'The debugger is switched off').

'$show_debugger_mode1'(trace) :-
	write(debugger_output, 'The debugger will first creep -- '),
	write(debugger_output, 'showing everything (trace)').

'$show_debugger_mode1'(debug) :-
	write(debugger_output, 'The debugger will first leap -- '),
	write(debugger_output, 'showing spypoints (debug)').




debugging :-
	set_bip_name(debugging, 0),
	'$show_debugger_mode',
	'$show_leashing_info',
	'$show_undefined_action',
	'$show_spy_points'.




leash(L) :-
	set_bip_name(leash, 1),
	var(L), !,
	'$pl_err_instantiation'.


leash(full) :-
	!,
	leash([call, exit, redo, fail, exception]).
leash(half) :-
	!,
	leash([call, redo]).
leash(loose) :-
	!,
	leash([call]).
leash(none) :-
	!,
	leash([]).
leash(tight) :-
	!,
	leash([call, redo, fail, exception]).

leash(L) :-
	'$leash_make_mask'(L, LeashMask), !,
	g_assign('$debug_leash', LeashMask),
	'$show_leashing_info'.

leash(L) :-
	'$pl_err_domain'(leash_ports, L).




'$leash_make_mask'([], 0).

'$leash_make_mask'([Port|L], LeashMask1) :-
	'$leash_make_mask'(L, LeashMask),
	'$debug_port_mask'(Port, Mask),
	LeashMask1 is LeashMask \/ Mask.




'$show_leashing_info' :-
	g_read('$debug_leash', LeashMask),
	'$show_leashing_info1'(LeashMask),
	nl(debugger_output).


'$show_leashing_info1'(0) :-
	!,
	write(debugger_output, 'No leashing').

'$show_leashing_info1'(LeashMask) :-
	write(debugger_output, 'Using leashing stopping at '),
	g_assign('$debug_work', 91),
	'$show_leashing_info2'(LeashMask),
	write(debugger_output, '] ports').


'$show_leashing_info2'(LeashMask) :-
	'$debug_port_mask'(Port, Mask),
	LeashMask /\ Mask > 0,
	g_read('$debug_work', C),
	g_assign('$debug_work', 44),
	format(debugger_output, '~c~a', [C, Port]),
	fail.

'$show_leashing_info2'(_).




'$debug_is_not_leashed'(Port) :-
	g_read('$debug_leash', LeashMask),
	'$debug_port_mask'(Port, Mask),
	Mask /\ LeashMask =:= 0 .




'$debug_port_mask'(call, 1).

'$debug_port_mask'(exit, 2).

'$debug_port_mask'(redo, 4).

'$debug_port_mask'(fail, 8).

'$debug_port_mask'(exception, 16).




'$show_undefined_action' :-
	current_prolog_flag(unknown, Action),
	write(debugger_output, 'Undefined predicates will '),
	'$show_undefined_action1'(Action),
	nl(debugger_output).


'$show_undefined_action1'(error) :-
	write(debugger_output, 'raise an existence_error').

'$show_undefined_action1'(warning) :-
	write(debugger_output, 'display a warning message and fail').

'$show_undefined_action1'(fail) :-
	write(debugger_output, fail).




:- meta_predicate(spypoint_condition(:, ?, ?)).

spypoint_condition(MGoal, Port, Test) :-
	set_bip_name(spypoint_condition, 3),
	'$strip_module_nonvar'(MGoal, Module, Goal),
	'$spypoint_condition1'(Goal, Module, Port, Test).


'$spypoint_condition1'(Goal, _, _, _) :-
	var(Goal), !,
	'$pl_err_instantiation'.

'$spypoint_condition1'(Goal, Module, Port, Test) :-
	callable(Goal), !,
	'$spypoint_condition2'(Goal, Module, Port, Test).

'$spypoint_condition1'(Goal, _, _, _) :-
	'$pl_err_type'(callable, Goal).


'$spypoint_condition2'(Goal, HomeModule, Port, Test) :-
	functor(Goal, F, A),
	(   '$get_module_of_pred'(HomeModule, F, A, Module) ->
	    '$debug_spy_set'([Module:F/A], c(Goal, Port, Test)),
	    g_read('$debug_mode', nodebug),
	    debug
	;
	    '$debug_qualify_pi_pretty'(F, A, HomeModule, PI),
	    format(debugger_output, 'Warning: The predicate ~q is undefined~n', [PI])
	),
	fail.

'$spypoint_condition2'(_, _, _, _).




:- meta_predicate(spy(:)).

spy(MSpec) :-
	set_bip_name(spy, 1),
	'$strip_module_nonvar'(MSpec, Module, Spec),
	'$debug_list_of_pred'(Spec, Module, L),
	'$debug_spy_set'(L, _),
	L \== [],
	g_read('$debug_mode', nodebug),
	debug,
	fail.

spy(_).




:- meta_predicate(nospy(:)).

nospy(MSpec) :-
	set_bip_name(nospy, 1),
	'$strip_module_nonvar'(MSpec, Module, Spec),
	'$debug_list_of_pred'(Spec, Module, L),
	'$debug_spy_reset'(L),
	fail.

nospy(_).




nospyall :-
	set_bip_name(nospyall, 0),
	retractall('$debug_spy_point'(_, _, _, _)),
	write(debugger_output, 'All spypoints removed'),
	nl(debugger_output),
	fail.

nospyall.




'$debug_spy_set'([], _).

'$debug_spy_set'([Module:F/A|L], Cond) :-
	(   retract('$debug_spy_point'(F, A, Module, _)) ->
	    Msg = 'There is already a spypoint on'
	;   Msg = 'Spypoint placed on'
	),
	assertz('$debug_spy_point'(F, A, Module, Cond)),
	'$debug_qualify_pi_pretty'(F, A, Module, PI),
	format(debugger_output, '~a ~q~n', [Msg, PI]), !,
	'$debug_spy_set'(L, Cond).




'$debug_spy_reset'([]).

'$debug_spy_reset'([Module:F/A|L]) :-
	(   retract('$debug_spy_point'(F, A, Module, _)) ->
	    Msg = 'Spypoint removed from'
	;   Msg = 'There is no spypoint on'
	),
	'$debug_qualify_pi_pretty'(F, A, Module, PI),
	format(debugger_output, '~a ~q~n', [Msg, PI]), !,
	'$debug_spy_reset'(L).




'$has_spy_point'(Goal, Module, Cond) :-
	functor(Goal, F, A),
	clause('$debug_spy_point'(F, A, Module, Cond), _), !.



'$has_no_spy_point'(Goal, Module) :-
	functor(Goal, F, A),
	clause('$debug_spy_point'(F, A, Module, _), _), !,
	fail.

'$has_no_spy_point'(_, _).




'$spy_test_condition'(Goal, Module, Port, c(Goal, Port, Test)) :-
%format('Spypoint test condition: goal ~q:~q  port: ~w  test: ~q~n', [Module, Goal, Port, Test]),
	(   var(Test) ->
	    true
	;
	    '$call_no_debug'(Test, Module, spy_conditional, 1), !
	).




'$show_spy_points' :-
	write(debugger_output, 'Spypoints:'),
	nl(debugger_output),
	clause('$debug_spy_point'(F, A, Module, _), _),
	'$debug_qualify_pi_pretty'(F, A, Module, PI),
	format(debugger_output, '   ~q~n', [PI]),
	fail.

'$show_spy_points'.




'$debug_list_of_pred'(Spec, _, _) :-
	var(Spec), !,
	'$pl_err_instantiation'.

'$debug_list_of_pred'([], _, []) :-
	!.

'$debug_list_of_pred'([Spec|Specs], Module, L) :-
	'$debug_list_of_pred_one'(Spec, Module, L1),
	'$debug_list_of_pred'(Specs, Module, L2),
	append(L1, L2, L), !.

'$debug_list_of_pred'(Spec, Module, L) :-
	'$debug_list_of_pred_one'(Spec, Module, L).


'$debug_list_of_pred_one'(Spec, _, _) :-
	var(Spec), !,
	'$pl_err_instantiation'.

'$debug_list_of_pred_one'(MSpec, _, L) :-
	functor(MSpec, :, 2), !,
	'$strip_module_nonvar'(MSpec, Module, Spec),
	'$debug_list_of_pred_one'(Spec, Module, L).

'$debug_list_of_pred_one'(F/A1 - A2, Module, L) :-% be cool ! accept F/(A1-A2) and (F/A1)-A2
	atom(F), !,
	'$debug_list_of_pred_one1'(F, A1, A2, Module, L).

'$debug_list_of_pred_one'(F/(A1 - A2), Module, L) :- % be cool ! accept F/(A1-A2) and (F/A1)-A2
	atom(F), !,
	'$debug_list_of_pred_one1'(F, A1, A2, Module, L).

'$debug_list_of_pred_one'(F, Module, L) :-
	atom(F), !,
	current_prolog_flag(max_arity, Max),
	'$debug_list_of_pred_one1'(F, 0, Max, Module, L).

'$debug_list_of_pred_one'(PI, Module, L) :-
	'$get_pred_indic'(PI, Module, Module1, F, A), % Module = Module1 in fact
	'$debug_list_of_pred_one1'(F, A, A, Module1, L).




'$debug_list_of_pred_one1'(F, _, _, _, _) :-
	var(F), !,
	'$pl_err_instantiation'.

'$debug_list_of_pred_one1'(_, A1, _, _, _) :-
	var(A1), !,
	'$pl_err_instantiation'.

'$debug_list_of_pred_one1'(_, _, A2, _, _) :-
	var(A2), !,
	'$pl_err_instantiation'.

'$debug_list_of_pred_one1'(F, A1, A2, Module, L) :-
	current_prolog_flag(max_arity, Max),
	integer(A1),
	integer(A2),
	A1 >= 0,
	A1 =< Max,
	A2 >= 0,
	A2 =< Max,
	g_assign('$debug_work', []),
	(   '$current_predicate_any'(Module:F/A),
	    A >= A1,
	    A =< A2,
	    g_read('$debug_work', X),
	    '$get_module_of_pred'(Module, F, A, Module1),
	    g_assign('$debug_work', [Module1:F/A|X]),
	    fail
	;   g_read('$debug_work', L)
	),
	(   L = [],
	    (   A1 = A2 ->
	        Z0 = F/A1
	    ;
		A1 = 0 ->
	        A2 = Max,
	        Z0 = F/any
	    ;
		Z0 = F/A1 - A2
	    ),
	    (   var(Module) ->
		Z = Z0
	    ;
		Z = Module:Z0
	    ),
	    format(debugger_output, 'Warning: spy ~q - no matching predicate~n', [Z]),
	    fail
	;
	    true
	).




          % The debugger:
          %
          % '$debug_call'/2 is called by meta-call (cf bc_supp.c) when the
          % debugger is active, ie. Set_Debug_Call_Code() has been called

'$debug_call'(notrace, _, _) :-
	!,
	notrace.

'$debug_call'(nodebug, _, _) :-
	!,
	nodebug.

'$debug_call'(trace, _, _) :-
	!,
	trace.

'$debug_call'(debug, _, _) :-
	!,
	debug.

'$debug_call'(debugging, _, _) :-
	!,
	debugging.

'$debug_call'(leash(L), _, _) :-
	!,
	leash(L).

'$debug_call'(spy(Spec), Module, _) :-
	!,
	spy(Module:Spec).

'$debug_call'(spypoint_condition(Goal, Port, Test), Module, _) :-
	!,
	spypoint_condition(Module:Goal, Port, Test).

'$debug_call'(nospy(Spec), Module, _) :-
	!,
	nospy(Module:Spec).

'$debug_call'(nospyall, _, _) :-
	!,
	nospyall.


'$debug_call'(Goal, Module0, CallInfo) :-
%format('$debug_call Goal:~w  Module: ~w~n',[Goal, Module0]),
	(   '$get_module_of_goal_if_not_meta_pred'(Module0, Goal, Module)
	    -> true
	;
	    Module = Module0
	),
	g_read('$debug_info', DebugInfo),
	DebugInfo = d(Invoc, OldAncLst),
	(   OldAncLst = [] ->
	    Index = 0
	;   OldAncLst = [a(_, _, _, Index, _)|_]
	),
	Invoc1 is Invoc + 1,
	Index1 is Index + 1,
	'$get_current_B'(B),
	NewAncLst = [a(Goal, Module, Invoc1, Index1, B)|OldAncLst],
	setarg(1, DebugInfo, Invoc1),
	setarg(2, DebugInfo, NewAncLst),
%format('starting of call ~w',[Module:Goal]), disp_B(''),
	'$debug_call1'(Goal, Module, CallInfo, Invoc1, Index1, NewAncLst, DebugInfo, Invoc, OldAncLst).



%disp_B(Msg):- '$get_current_B'(B), format(' ~w B:%d (%#x)\n', [Msg, B, B]).


'$debug_call1'(Goal, Module, CallInfo, Invoc1, Index1, NewAncLst, DebugInfo, _, OldAncLst) :-
%format('goal is ~w  Call Info:~w~n', [Module:Goal, CallInfo]),
	'$get_current_B'(B),
	'$catch_internal'('$debug_call_port'(Goal, Module, CallInfo, Invoc1, Index1, NewAncLst), Ball, '$debug_exception_port'(Goal, Module, Invoc1, Index1, NewAncLst, Ball), system, 0),
	'$get_current_B'(B1),
%format(' after effective call: ~w  B(start):%#x B1(end):%#x~n', [Module:Goal, B, B1]),
%disp_B('before end call'),
	'$debug_end_call'(Goal, Module, Invoc1, Index1, NewAncLst, DebugInfo, OldAncLst),
%disp_B('after end call and before test determin'),
	(   B1 =< B, !
	;   true
	).
%disp_B('after cut if determin').


'$debug_call1'(Goal, Module, _, Invoc1, Index1, NewAncLst, _, _, _) :-
%format('the call to ~w failed~n', [Module:Goal]),	
	'$debug_port'(Goal, Module, Invoc1, Index1, NewAncLst, fail),
	fail.

'$debug_call1'(Goal, Module, CallInfo, Invoc1, _, _, DebugInfo, Invoc, OldAncLst) :-
	g_read('$debug_next', retry(X)), % if user asked a 'retry'
	X >= Invoc1,
	setarg(1, DebugInfo, Invoc),
	setarg(2, DebugInfo, OldAncLst),
	g_assign('$debug_next', trace),
	'$debug_call'(Goal, Module, CallInfo).




'$debug_call_port'(Goal, Module, CallInfo, Invoc, Index, AncLst) :-
	g_assign('$debug_unify', ''),
	'$debug_port'(Goal, Module, Invoc, Index, AncLst, call),
	g_read('$debug_unify', DebugUnify),
	(   DebugUnify == '' ->
%format('now I call ~w~n', [Module:Goal]),
	    Goal \== fail,	% NB: bc_supp.c calls the debugger for 'fail/0'.
				% but don't call 'call_from_debugger since it is a
				% control-construct (thus its native codep == NULL)
	    '$call_from_debugger'(Goal, Module, CallInfo)
%,g_read('$debug_next', DebugNext),format('now I exit ~w   debug_next:~w~n', [Module:Goal,DebugNext])
	;
	    Goal = DebugUnify
	).




'$debug_end_call'(Goal, Module, Invoc1, Index1, AncLst, DebugInfo, OldAncLst) :-
%write('in end call'(Goal)),nl,
	'$debug_port'(Goal, Module, Invoc1, Index1, AncLst, exit),
	setarg(2, DebugInfo, OldAncLst).

'$debug_end_call'(Goal, Module, Invoc1, Index1, AncLst, _, _) :-
	'$debug_port'(Goal, Module, Invoc1, Index1, AncLst, redo),
	fail.




'$debug_exception_port'(Goal, Module, Invoc, Index, AncLst, Ball) :-
	g_assign('$debug_ball', Ball),
	'$debug_port'(Goal, Module, Invoc, Index, AncLst, exception),
	throw(Ball).




          % debug_port

'$debug_port'(Goal, Module, Invoc, Index, AncLst, Port) :-
	'$get_current_B'(B),
	'$debug_port1'(Goal, Module, Invoc, Index, AncLst, Port, B).


'$debug_port1'(Goal, Module, Invoc, Index, AncLst, Port, B) :-
	'$debug_port2'(Goal, Module, Invoc, Index, AncLst, Port, B),
	fail.

'$debug_port1'(_, _, _, _, _, _, _) :-
	g_read('$debug_next', DebugNext),              % fail for 'r' and 'f'
%format('$debug_next = ~w~n', [DebugNext]),
	atom(DebugNext).




'$debug_port2'(Goal, Module, Invoc, _, _, Port, _) :-
	g_read('$debug_next', DebugNext),
%format(' test port_ignore   debug_next:~w~n', [DebugNext]),
'$debug_port_ignore'(DebugNext, Goal, Module, Invoc, Port), !.

'$debug_port2'(Goal, Module, Invoc, Index, AncLst, Port, B) :-
	'$debug_port_prompt'(Goal, Module, Invoc, Index, AncLst, Port, B).




'$debug_port_ignore'(nodebug, _, _, _, _).

'$debug_port_ignore'(debug, Goal, Module, _, _) :-
	'$has_no_spy_point'(Goal, Module).

'$debug_port_ignore'(debug, Goal, Module, _, Port) :-
	'$has_spy_point'(Goal, Module, Cond),
%format('Port: ~w spypoint on ~w:~q with condition: ~q~n', [Port, Module, Goal, Cond]),
	(   '$spy_test_condition'(Goal, Module, Port, Cond) ->
	    fail
	;   true
	).

'$debug_port_ignore'(skip, _, _, Invoc, Port) :-
	g_read('$debug_skip', s(Invoc1, Mask1)),
	'$debug_port_mask'(Port, Mask),
	(   Invoc = Invoc1,
	    Mask /\ Mask1 > 0 ->
	    fail
	;   !
	).

'$debug_port_ignore'(fail(Invoc1), _, _, Invoc, Port) :-
	(   Invoc > Invoc1
	;   Port \== fail
	), !.

'$debug_port_ignore'(retry(_), _, _, _, _).




'$debug_port_prompt'(Goal, Module, Invoc, Index, AncLst, Port, B) :-
	repeat,
	g_assign('$debug_next', trace),
	'$debug_write_goal'(Goal, Module, Invoc, Index, Port),
	(   '$has_no_spy_point'(Goal, Module),
	    '$debug_is_not_leashed'(Port) ->
	    nl(debugger_output)
	;   '$debug_read_cmd'(C),
	    '$debug_exec_cmd'(C, Goal, Module, Invoc, AncLst, Port, B)
	), !.




'$debug_read_cmd'(C) :-
	write(debugger_output, ' ? '),
	flush_output(debugger_output),
	get_key(debugger_input, X),
	X >= 0,
	X < 255,
	char_code(C, X),
	format(debugger_output, '~N', []).




'$debug_read_integer'(X) :-
	read_integer(debugger_input, X),
	repeat,
	get_code(debugger_input, 10), !.				% the last '\n'




'$debug_exec_cmd'(C, Goal, Module, Invoc, AncLst, Port, B) :-
	char_code(C, X),
	X >= 10,
	X =< 13,
	'$debug_exec_cmd'(c, Goal, Module, Invoc, AncLst, Port, B).

'$debug_exec_cmd'(c, _, _, _, _, _, _) :-				% creep
	g_assign('$debug_next', trace).

'$debug_exec_cmd'(l, _, _, _, _, _, _) :-				% leap
	g_assign('$debug_next', debug).

'$debug_exec_cmd'(s, Goal, Module, Invoc, AncLst, Port, B) :-		% skip
	(   Port = exit
	;   Port = fail
	;   Port = exception
	), !,
	'$debug_exec_cmd'(c, Goal, Module, Invoc, AncLst, Port, B).

'$debug_exec_cmd'(s, _, _, Invoc, _, _, _) :-				% skip
	g_assign('$debug_next', skip),
	'$debug_port_mask'(exit, Mask1),
	'$debug_port_mask'(fail, Mask2),
	'$debug_port_mask'(exception, Mask3),
	Mask is Mask1 \/ Mask2 \/ Mask3,
	g_assign('$debug_skip', s(Invoc, Mask)).

'$debug_exec_cmd'('G', _, _, _, _, _, _) :-				% goto
	write(debugger_output, 'Inovcation nb: '),
	'$debug_read_integer'(Invoc),
	g_assign('$debug_next', skip),
	g_assign('$debug_skip', s(Invoc, 31)).

'$debug_exec_cmd'(r, _, _, _, _, call, _) :-				% retry
	!,
	fail.

'$debug_exec_cmd'(r, _, _, Invoc, _, _, _) :-				% retry
	g_assign('$debug_next', retry(Invoc)).

'$debug_exec_cmd'(f, _, _, _, _, fail, _) :-				% fail
	!,
	fail.

'$debug_exec_cmd'(f, _, _, Invoc, _, _, _) :-				% fail
	g_assign('$debug_next', fail(Invoc)).

'$debug_exec_cmd'(w, Goal, Module, _, _, _, _) :-			% write
	'$debug_qualify_goal_pretty'(Goal, Module, Goal1),
	write(debugger_output, '                    '),
	write(debugger_output, Goal1),
	nl(debugger_output), !,
	fail.

'$debug_exec_cmd'(d, Goal, Module, _, _, _, _) :-			% display
	'$debug_qualify_goal_pretty'(Goal, Module, Goal1),
	write(debugger_output, '                    '),
	display(debugger_output, Goal1),
	nl(debugger_output), !,
	fail.

'$debug_exec_cmd'(p, Goal, Module, _, _, _, _) :-			% print
	'$debug_qualify_goal_pretty'(Goal, Module, Goal1),
	write(debugger_output, '                    '),
	print(debugger_output, Goal1),
	nl(debugger_output), !,
	fail.

'$debug_exec_cmd'(e, _, _, _, _, Port, _) :-				% exception
	(   Port = exception ->
	    g_read('$debug_ball', Ball),
	    format(debugger_output, 'Exception raised: ~q~n', [Ball])
	;   write(debugger_output, 'Option not applicable at this port'),
	    nl(debugger_output)
	), !,
	fail.

'$debug_exec_cmd'(g, _, _, _, AncLst, _, _) :-				% ancestors
	'$debug_disp_anc_lst'(AncLst), !,
	fail.

'$debug_exec_cmd'('A', _, _, _, AncLst, _, B) :-			% alternatives
	'$debug_disp_alternatives'(AncLst, B), !,
	fail.

'$debug_exec_cmd'(u, _, _, _, _, Port, _) :-				% unify
	(   Port = call ->
	    write(debugger_output, 'Head: '),
	    read(debugger_input, DebugUnify),
	    g_assign('$debug_unify', DebugUnify)
	;   write(debugger_output, 'Option not applicable at this port'),
	    nl(debugger_output), !,
	    fail
	).

'$debug_exec_cmd'(n, _, _, _, _, _, _) :-				% nodebug
	'$debug_switch_off'.

'$debug_exec_cmd'(=, _, _, _, _, _, _) :-				% debugging
	debugging, !,
	fail.

'$debug_exec_cmd'('.', Goal, Module, _, _, _, _) :-			% father file
	!,
	functor(Goal, F, A),
	(   '$get_predicate_file_info'(Module, F, A, PlFile, PlLine) ->
	    format(debugger_output, '~a/~d defined in ~a:~d~n', [F, A, PlFile, PlLine])
	;
	    format(debugger_output, 'no file information for ~a/~d~n', [F, A])
	),
	fail.

'$debug_exec_cmd'(+, Goal, Module, _, _, _, _) :-			% spy this
	functor(Goal, F, A),
	spy(Module:F/A), !,
	fail.

'$debug_exec_cmd'(*, Goal, Module, _, _, _, _) :-			% spy conditionally
	repeat,
	write(debugger_output, 'Goal,Port,Test: '),
	read(debugger_input, (Goal1, Port1, Test1)),
	callable(Goal1),
	functor(Goal, F, A),
	functor(Goal1, F, A),
	spypoint_condition(Module:Goal1, Port1, Test1), !,
	fail.

'$debug_exec_cmd'(-, Goal, Module, _, _, _, _) :-			% nospy this
	functor(Goal, F, A),
	nospy(Module:F/A), !,
	fail.

'$debug_exec_cmd'('L', Goal, Module, _, _, _, _) :-			% listing
	!,
	functor(Goal, F, A),
	PI = Module:F/A,
	(   '$current_predicate_any'(PI) ->
	    (   '$predicate_property1'(Module, F, A, native_code) ->
	        format(debugger_output, 'native code predicate ~a/~d~n', [F, A])
	    ;
		'$call_c'('Pl_Reset_Debug_Call_Code_0'),
		'$listing_any'(PI),
	        nl(debugger_output),
		'$call_c'('Pl_Set_Debug_Call_Code_0')
	    )
	;   format(debugger_output, 'cannot find any info on ~a/~d~n', [F, A])
	),
	fail.

'$debug_exec_cmd'(a, _, _, _, _, _, _) :-				% abort
	abort.

'$debug_exec_cmd'(b, _, _, _, _, _, _) :-				% break
	break, !,
	fail.

'$debug_exec_cmd'(@, _, _, _, _, _, _) :-				% command
	write(debugger_output, 'Command: '),
	read(debugger_input, Command),
	(   '$catch_no_debug'(Command, Err, format(debugger_output, 'Warning: ~q - exception raised ~w~n', [Command, Err]), user, debugger_exec_cmd, 1) ->
	    true
	;   format(debugger_output, 'Warning: ~q - goal failed~n', [Command])
	), !,
	fail.

'$debug_exec_cmd'(<, _, _, _, _, _, _) :-				% print depth
	write(debugger_output, 'Print Depth: '),
	'$debug_read_integer'(Depth),
	g_assign('$debug_depth', Depth), !,
	fail.

'$debug_exec_cmd'(?, _, _, _, _, _, _) :-				% help
	'$debug_disp_help', !,
	fail.

'$debug_exec_cmd'(h, _, _, _, _, _, _) :-				% help
	'$debug_disp_help', !,
	fail.

'$debug_exec_cmd'('W', _, _, _, _, _, _) :-				% WAM debugger
	wam_debug, !,
	fail.


'$debug_exec_cmd'(_, _, _, _, _, _, _) :-                                % error
	write(debugger_output, 'Unknown command (type h for help)'),
	nl(debugger_output),
	fail.




'$debug_write_goal'(Goal, Module, Invoc, Index, Port) :-
	g_read('$debug_depth', Depth),
	(   '$has_no_spy_point'(Goal, Module) ->
	    Indic = ' '
	;   Indic = (+)
	),
	'$debug_port_pretty'(Port, Port1),
	format(debugger_output, '~N ~a %4d %4d  ~a', [Indic, Invoc, Index, Port1]),
	'$debug_qualify_goal_pretty'(Goal, Module, Goal1),
	write_term(debugger_output, Goal1, [quoted(true), max_depth(Depth)]).



'$debug_qualify_pi_pretty'(F, A, Module, PI) :-
%	'$get_module_of_pred'(Module, F, A, Module1),
	'$debug_needs_module'(Module), !,
	PI = Module:F/A.

'$debug_qualify_pi_pretty'(F, A, _, F/A).





'$debug_qualify_goal_pretty'(Goal, Module, Goal1) :-
%	'$get_module_of_goal'(Module, Goal, Module1),
	'$debug_needs_module'(Module), !,
	Goal1 = Module:Goal.

'$debug_qualify_goal_pretty'(Goal, _, Goal).



'$debug_needs_module'(Module) :-
	Module \== user,
	Module \== system.




'$debug_port_pretty'(call, 'Call: ').

'$debug_port_pretty'(redo, 'Redo: ').

'$debug_port_pretty'(fail, 'Fail: ').

'$debug_port_pretty'(exit, 'Exit: ').

'$debug_port_pretty'(exception, 'Exception: ').

'$debug_port_pretty'(no_port, '').




'$debug_disp_anc_lst'([a(_, _, _, _, _)|AncLst]) :-
	write(debugger_output, 'Ancestors:'),
	nl(debugger_output),
	'$debug_disp_anc_lst1'(AncLst).


'$debug_disp_anc_lst1'([]).

'$debug_disp_anc_lst1'([a(Goal, Module, Invoc, Index, _)|AncLst]) :-
	'$debug_disp_anc_lst1'(AncLst),
	'$debug_write_goal'(Goal, Module, Invoc, Index, no_port).




'$debug_disp_alternatives'(AncLst, B) :-
	write(debugger_output, 'Alternatives:'),
	nl(debugger_output),
	'$debug_disp_alternatives1'(AncLst, B),
	format(debugger_output, '~N--------------~n', []).


'$debug_disp_alternatives1'([], _).

'$debug_disp_alternatives1'([a(Goal, Module, Invoc, Index, B1)|AncLst], B2) :-
	'$debug_disp_alternatives1'(AncLst, B1),
	'$debug_write_goal'(Goal, Module, Invoc, Index, no_port),
	'$debug_disp_alt'(B2, B1).


'$debug_disp_alt'(B, B) :-
	!.

'$debug_disp_alt'(BFrom, BTo) :-
	'$choice_point_info'(BFrom, Module, F, A, B1),
	'$debug_disp_alt'(B1, BTo),
	'$pred_without_aux'(F, A, F1, A1),
	'$debug_disp_alt1'(Module, F1, A1, BFrom), !.


'$debug_disp_alt1'(system, F, _, _) :-                      % hide debug alternatives
	'$debug_is_debug_predicate'(F).

'$debug_disp_alt1'(_, _, _, B) :-                           % dynam clause selection ?
	'$call_c_test'('Pl_Scan_Choice_Point_Info_4'(B, Module, F, A)),    % fail if not
	'$pred_without_aux'(F, A, F1, A1),
	'$debug_qualify_pi_pretty'(F1, A1, Module, PI),
	'$debug_disp_alt2'(PI).

'$debug_disp_alt1'(system, '$catch_internal1', 6, B) :-            % hide debug catch
	'$choice_point_arg'(B, 1, Goal),
	functor(Goal, F, _),
	'$debug_is_debug_predicate'(F).

'$debug_disp_alt1'(system, '$trail_handler', 1, _).

'$debug_disp_alt1'(Module, F, A, _) :-                      % detect system predicate
	sub_atom(F, 0, 1, _, $),
	'$predicate_property1'(Module, F, A, native_code),
	(   (   sub_atom(F, 1, _, 4, F1),
	        '$debug_check_bip'(F1, A1)
	    ;
		sub_atom(F, 1, _, 1, F1),
	        '$debug_check_bip'(F1, A1)
	    ) ->
	    '$debug_disp_alt2'(F1/A1)
	;
	    '$debug_disp_alt2'('system predicate'(F/A))
	).

'$debug_disp_alt1'(Module, F, A, _) :-                             % normal predicate
	'$debug_qualify_pi_pretty'(F, A, Module, PI),
	'$debug_disp_alt2'(PI).


'$debug_disp_alt2'(X) :-
	format(debugger_output, '~N   1 choice-point for ~w~n', [X]).




'$debug_is_debug_predicate'(F) :-
	sub_atom(F, 0, 7, _, '$debug_').




'$debug_check_bip'(F1, A1) :-
	'$predicate_property1'(user, F1, A1, built_in).

/* useless since now built_in_fd ==> built_in
'$debug_check_bip'(F1, A1) :-
	'$predicate_property1'(user, F1, A1, built_in_fd).
*/




'$debug_disp_help' :-
	format(debugger_output, 'Debugging commands:~n~n', []),
	format(debugger_output, 'RET/c creep        l  leap ~n', []),
	format(debugger_output, '   s  skip         G  goto~n', []),
	format(debugger_output, '   r  retry        f  fail~n', []),
	format(debugger_output, '   w  write        d  display~n', []),
	format(debugger_output, '   p  print        e  exception~n', []),
	format(debugger_output, '   g  ancestors    A  alternatives~n', []),
	format(debugger_output, '   u  unify        .  father file~n', []),
	format(debugger_output, '   n  nodebug      =  debugging~n', []),
	format(debugger_output, '   +  spy this     *  spy conditionally~n', []),
	format(debugger_output, '   -  nospy this   L  listing~n', []),
	format(debugger_output, '   a  abort        b  break~n', []),
	format(debugger_output, '   @  command      <  set printdepth~n', []),
	format(debugger_output, '  h/? help         W  WAM debugger~n', []),
	nl(debugger_output).





'$choice_point_info'(B, M, F, A, LastB) :-
	'$call_c'('Pl_Choice_Point_Info_5'(B, M, F, A, LastB)).


'$choice_point_arg'(B, I, Arg) :-
	'$call_c'('Pl_Choice_Point_Arg_3'(B, I, Arg)).
