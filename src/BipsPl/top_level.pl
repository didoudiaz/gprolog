/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : top_level.pl                                                    *
 * Descr.: top Level                                                       *
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

:-	ensure_linked([consult / 1, load / 1]).


top_level :-
	current_prolog_flag(prolog_name, Name),
	current_prolog_flag(prolog_version, Version),
	current_prolog_flag(prolog_copyright, Copyright),
	current_prolog_flag(address_bits, Bits),
	current_prolog_flag(compiled_at, Date),
	current_prolog_flag(c_cc, CC),
	format(top_level_output, '~N~a ~a (~d bits)~n', [Name, Version, Bits]),
	format(top_level_output, 'Compiled ~a with ~a~n', [Date, CC]),
	write(top_level_output, 'By Daniel Diaz'),
	nl,
	format(top_level_output, '~a~n', [Copyright]),
	break.




break :-
	'$call_c'('Pl_Set_Ctrl_C_Handler_0'),
	'$sys_var_read'(10, Level),
	'$sys_var_read'(11, B),
	g_read('$all_solutions', All),
	(   Level > 0 ->
	    format(top_level_output, '~N{Break Level ~d}~n', [Level])
	;   true
	),
	'$sys_var_inc'(10),
	g_read('$cmd_line_consult_file', LFile),
	'$exec_cmd_line_consult_files'(LFile),
	g_read('$cmd_line_entry_goal', LGoal),
	'$exec_cmd_line_entry_goals'(LGoal),
	g_assign('$cmd_line_entry_goal', []),
	'$top_level1',
	'$sys_var_dec'(10),
	'$sys_var_write'(11, B),
	g_assign('$all_solutions', All),
	(   Level > 0 ->
	    format(top_level_output, '~N{End Break}~n', [Level])
	;   true
	).




'$top_level1' :-
	repeat,
	'$catch_internal'('$top_level2', X, '$top_level_exception'(X), false), !.




'$top_level_abort' :-
	'$reinit_after_exception',
	'$sys_var_read'(11, B),
	write(top_level_output, 'execution aborted\n'),
	'$catch_sync_for_fail_at'(B).




'$top_level_stop' :-
	'$reinit_after_exception',
	'$sys_var_read'(11, B),
	'$catch_sync_for_fail_at'(B).




'$top_level_exception'('$post_query_exception'(X)) :-
	'$reinit_after_exception',
	!,
	format(top_level_output, '~Ntop-level exception: ', []),
	write_term(top_level_output, X, [quoted(true), numbervars(false), namevars(false)]),
	nl(top_level_output),
	fail.
	

'$top_level_exception'(X) :-
	'$reinit_after_exception',
	format(top_level_output, '~Nuncaught exception: ', []),
	write_term(top_level_output, X, [quoted(true), numbervars(false), namevars(false)]),
	nl(top_level_output),
	fail.



'$reinit_after_exception' :-
% g_read('$char_conv', CharConv),
% set_prolog_flag(char_conversion, CharConv),
	(   '$sys_var_read'(12, 1) ->
	    g_read('$user_prompt', UserPrompt),
	    '$set_linedit_prompt'(UserPrompt)
	;   true
	).




'$top_level2' :-
	repeat,
	'$get_current_B'(B),   % the current choice-point
%	'$sys_var_read'(7, B), % the last Handler created by catch/3 (what is better ???)
	'$sys_var_write'(11, B),
%write('top-level catcher'(B)), nl,
	'$write_indicator',
% current_prolog_flag(char_conversion, CharConv),
% g_assign('$char_conv', CharConv),
% set_prolog_flag(char_conversion, off),
	Prompt = '| ?- ',
	(   '$sys_var_read'(12, 1) ->
	    '$get_linedit_prompt'(UserPrompt),
	    g_assign('$user_prompt', UserPrompt),
	    '$set_linedit_prompt'(Prompt)
	;   write(top_level_output, Prompt)
	),
	flush_output(top_level_output),
	'$read_query'(X, QueryVars),
	(   '$sys_var_read'(12, 1) ->
	    '$set_linedit_prompt'(UserPrompt)
	;   true
	),
% set_prolog_flag(char_conversion, CharConv),
	sort(QueryVars, QueryVars1),
	(   X == end_of_file ->
	    nl(top_level_output), !
	;   user_time(Time0),
	    (   '$exec_query'(X, QueryVars1) ->
	        Ok = yes
	    ;   Ok = no
	    ),
	    user_time(Time1),
	    Time is Time1 - Time0,
	    format(top_level_output, '~N~n', []),
	    (   Time = 0 ->
	        true
	    ;   format(top_level_output, '(~d ms) ', [Time])
	    ),
	    format(top_level_output, '~a~n', [Ok]),
	    fail
	).




'$write_indicator' :-
	g_read('$debug_mode', DebugMode),
	'$dbg_indicator'(DebugMode, A),
	'$sys_var_read'(10, Level),
	(   Level > 1 ->
	    Level1 is Level - 1,
	    (   A = '' ->
	        format(top_level_output, '{~d}~n', [Level1])
	    ;   format(top_level_output, '{~a,~d}~n', [A, Level1])
	    )
	;   A = '' ->
	    true
	;   format(top_level_output, '{~a}~n', [A])
	),
	fail.

'$write_indicator'.




'$dbg_indicator'(trace, trace).

'$dbg_indicator'(debug, debug).

'$dbg_indicator'(nodebug, '').




'$read_query'(X, QueryVars) :-
	'$sys_var_read'(10, 1), % comment to execute in nested top-levels
	g_read('$cmd_line_query_goal', [Goal|LGoal]),
	g_assign('$cmd_line_query_goal', LGoal),
	!,
	Prompt = '| ?- ',
	(   '$sys_var_read'(12, 1) ->
	    write(top_level_output, Prompt)
	;   true),
	format(top_level_output, '~a.~n', [Goal]),
	read_term_from_atom(Goal, X, [end_of_term(eof), variable_names(QueryVars)]).

'$read_query'(X, QueryVars) :-
	read_term(top_level_input, X, [variable_names(QueryVars)]).




'$exec_query'(X, QueryVars) :-
	g_read('$debug_mode', DebugMode),
	g_assign('$debug_next', DebugMode),
	g_assign('$all_solutions', f),
	'$get_current_B'(B),
	'$call_c'('Pl_Save_Regs_For_Signal'),  % save some registers in case of CTRL+C
	'$call'(X, top_level, 0, true),
	'$call_c'('Pl_Save_Regs_For_Signal'),  % save some registers in case of CTRL+C
	'$get_current_B'(B1),
	format(top_level_output, '~N', []),
	'$catch_internal'('$set_query_vars_names'(QueryVars, ToDispVars), Err, throw('$post_query_exception'(Err)), false),
%	'$set_query_vars_names'(QueryVars, ToDispVars),
	(   fail,                             % do not activate 'alt if vars'
	    ToDispVars = [] ->
	    true                              % no alt if only anonymous vars
	;
	    '$write_solution'(ToDispVars, B1, B),
	    (   B1 > B ->
	        g_read('$all_solutions', f),          % fail for previous 'a'
	        write(top_level_output, ' ? '),
	        '$read_return'                        % fail for ';' ' ' and  'a'
	    ;   true
	    )
	).




'$set_query_vars_names'(QueryVars, ToDispVars1) :-
	name_query_vars(QueryVars, ToDispVars),
	'$remove_underscore_vars'(ToDispVars, ToDispVars1),
	name_singleton_vars(ToDispVars1),
	bind_variables(ToDispVars1, [exclude(QueryVars), namevars]).




'$remove_underscore_vars'([], []).

'$remove_underscore_vars'([Name = Term|ToDispVars], ToDispVars1) :-
	(   sub_atom(Name, 0, 1, _, '_')
	;   Term = '$VARNAME'(Name1),
	    sub_atom(Name1, 0, 1, _, '_')),
	!,
	'$remove_underscore_vars'(ToDispVars, ToDispVars1).

'$remove_underscore_vars'([X|ToDispVars], [X|ToDispVars1]) :-
	'$remove_underscore_vars'(ToDispVars, ToDispVars1).




'$write_solution'([], B1, B) :-
	!,
	(   B1 > B ->
	    format(top_level_output, '~ntrue', [])
	;   true
	).

'$write_solution'(ToDispVars, _, _) :-
	( current_op(Prior, xfx, =) ; Prior = 700), !,
	Prior1 is Prior - 1,
	'$write_solution1'(ToDispVars, Prior1).


'$write_solution1'([], _).

'$write_solution1'([Name = Value|ToDispVars], Prior) :-
	(   acyclic_term(Value) ->
	    format(top_level_output, '~n~a = ', [Name]),
	    write_term(top_level_output, Value, [quoted(true), numbervars(false), namevars(true), priority(Prior)])
	;
	    format(top_level_output, '~ncannot display cyclic term for ~a', [Name])
	),
	'$write_solution1'(ToDispVars, Prior).




'$read_return' :-
	flush_output(top_level_output),
	get_key(top_level_input, X),
	'$read_return'(X), !.




'$read_return'(10).		% newline

'$read_return'(13).		% carriage-return

'$read_return'(97) :-		% 'a'
	g_assign('$all_solutions', t), !,
	fail.

'$read_return'(59) :-		% ';'
	format(top_level_output, '~N', []), !,
	fail.

'$read_return'(32) :-		% ' ' (simulate a ';')
	format(top_level_output, '\b;~N', []), !,
	fail.

'$read_return'(_) :-
	nl(top_level_output),
	write(top_level_output, 'Action (; for next solution, a for all solutions, RET to stop) ? '),
	'$read_return'.



/* interface with command-line option consulting files */

'$exec_cmd_line_consult_files'([File|_LFile]) :-
	'$catch_internal'('$consult2'(File), error(Err, _), true, false),
	nonvar(Err),
	format('~Nwarning: command-line consulting file ~q failed due to ~q~n', [File, Err]),
	fail.
	
'$exec_cmd_line_consult_files'([_|LFile]) :-
	!,
	'$exec_cmd_line_consult_files'(LFile).

'$exec_cmd_line_consult_files'(_).		% can be another term than []


/* interface with command-line options executing goals */

'$exec_cmd_line_entry_goals'([Goal|LGoal]):-
	!,
	'$exec_cmd_line_goal'(Goal),
	'$exec_cmd_line_entry_goals'(LGoal).

'$exec_cmd_line_entry_goals'(_).		% can be another term than []




'$exec_cmd_line_goal'(Goal) :-		% called by top_level.c
	(   '$catch'('$exec_cmd1'(Goal), Err, '$exec_cmd_err'(Goal, Err), 'command-line', -1, false) ->
	    true
	;   format('~Nwarning: command-line goal ~q failed~n', [Goal])).


'$exec_cmd1'(Goal) :-
	read_term_from_atom(Goal, TermGoal, [end_of_term(eof)]),
	'$call'(TermGoal, 'command-line', -1, false).


'$exec_cmd_err'(Goal, Err) :-
	format('~Nwarning: command-line goal ~q caused exception: ~q~n', [Goal, Err]).

