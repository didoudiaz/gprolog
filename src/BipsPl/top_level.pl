/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog buit-in predicates                                       */
/* File  : top_level.pl                                                    */
/* Descr.: top Level                                                       */
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

:-	ensure_linked([consult/1, load/1]).


top_level :-
	current_prolog_flag(prolog_name, Name),
	current_prolog_flag(prolog_version, Version),
	current_prolog_flag(prolog_copyright, Copyright),
	format(top_level_output, '~N~a ~a~n', [Name, Version]),
	write('By Daniel Diaz'),
	nl,
	format(top_level_output, '~a~n', [Copyright]),
	break.




break :-
	'$call_c'('Set_Ctrl_C_Handler_0'),
	'$sys_var_read'(10, Level),
	'$sys_var_read'(11, B),
	g_read('$all_solutions', All),
	(   Level>0 ->
	    format(top_level_output, '~N{Break Level ~d}~n', [Level])
	;   true
	),
	'$sys_var_inc'(10),
	'$top_level1',
	'$sys_var_dec'(10),
	'$sys_var_write'(11, B),
	g_assign('$all_solutions', All),
	(   Level>0 ->
	    format(top_level_output, '~N{End Break}~n', [Level])
	;   true
	).




'$top_level1' :-
	repeat,
	'$catch_internal'('$top_level2', X, '$top_level_exception'(X), false), !.




'$top_level_abort' :-
	'$sys_var_read'(11, B),
	format(top_level_output, 'execution aborted~n', []),
	'$catch_sync_for_fail_at'(B).

	


'$top_level_stop' :-
	'$sys_var_read'(11, B),
	'$catch_sync_for_fail_at'(B).




'$top_level_exception'(X) :-
	(   '$sys_var_read'(12, 1) ->
	    g_read('$user_prompt', UserPrompt),
	    '$set_linedit_prompt'(UserPrompt)
	;   true
	),
% g_read('$char_conv',CharConv),
% set_prolog_flag(char_conversion,CharConv),
	format(top_level_output, '~Nuncaught exception: ', []),
	write_term(top_level_output, X, [quoted(true), numbervars(false)]),
	nl(top_level_output),
	fail.




'$top_level2' :-
	repeat,
	'$get_current_B'(B),
	'$sys_var_write'(11, B),
	'$write_indicator',
% current_prolog_flag(char_conversion,CharConv),
% g_assign('$char_conv',CharConv),
% set_prolog_flag(char_conversion,off),
	Prompt='| ?- ',
	(   '$sys_var_read'(12, 1) ->
	    '$get_linedit_prompt'(UserPrompt),
	    g_assign('$user_prompt', UserPrompt),
	    '$set_linedit_prompt'(Prompt)
	;   write(top_level_output, Prompt)
	),
	flush_output(top_level_output),
	read_term(top_level_input, X, [variable_names(QueryVars)]),
	(   '$sys_var_read'(12, 1) ->
	    '$set_linedit_prompt'(UserPrompt)
	;   true
	),
% set_prolog_flag(char_conversion,CharConv),
	sort(QueryVars, QueryVars1),
	(   X==end_of_file ->
	    nl(top_level_output), !
	;   user_time(Time0),
	    (   '$exec'(X, QueryVars1) ->
	        Ok=yes
	    ;   Ok=no
	    ),
	    user_time(Time1),
	    Time is Time1-Time0,
	    format(top_level_output, '~N~n', []),
	    (   Time=0 ->
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
	(   Level>1 ->
	    Level1 is Level-1,
	    (   A='' ->
	        format(top_level_output, '{~d}~n', [Level1])
	    ;   format(top_level_output, '{~a,~d}~n', [A, Level1])
	    )
	;   A='' ->
	    true
	;   format(top_level_output, '{~a}~n', [A])
	),
	fail.

'$write_indicator'.




'$dbg_indicator'(trace, trace).

'$dbg_indicator'(debug, debug).

'$dbg_indicator'(nodebug, '').




'$exec'(X, QueryVars) :-
	g_read('$debug_mode', DebugMode),
	g_assign('$debug_next', DebugMode),
	g_assign('$all_solutions', f),
	'$get_current_B'(B),
	'$call'(X, top_level, 0, true),
	'$get_current_B'(B1),
	format(top_level_output, '~N', []),
	(   fail,                                % do not activate 'alt if vars'
	    QueryVars=[] ->
	    true                                 % no alt if only anonymous vars
	;   name_query_vars(QueryVars, ToDispVars),
	    name_singleton_vars(ToDispVars),
	    bind_variables(ToDispVars, [exclude(QueryVars), namevars]),
	    '$write_solution'(ToDispVars, B1, B),
	    (   B1>B ->
	        g_read('$all_solutions', f),     % fail for previous 'a'
	        write(top_level_output, ' ? '),
	        '$read_return'                   % fail for ';' and  'a'
	    ;   true
	    )
	).




'$write_solution'([], B1, B) :-
	!,
	(   B1>B ->
	    format(top_level_output, '~ntrue', [])
	;   true
	).

'$write_solution'(ToDispVars, _, _) :-
	'$write_solution1'(ToDispVars).


'$write_solution1'([]).

'$write_solution1'([Name=Value|ToDispVars]) :-
	format(top_level_output, '~n~a = ', [Name]),
	write_term(top_level_output, Value, [quoted(true), numbervars(false)]),
	'$write_solution1'(ToDispVars).




'$read_return' :-
	flush_output(top_level_output),
	get_code_no_echo(top_level_input, X),
	'$read_return'(X), !.




'$read_return'(10).

'$read_return'(13).

'$read_return'(97) :-
	g_assign('$all_solutions', t), !,
	fail.

'$read_return'(59) :-
	format(top_level_output, '~N', []), !,
	fail.

'$read_return'(_) :-
	nl(top_level_output),
	write(top_level_output, 'Action (; for next solution, a for all solutions, RET to stop) ? '),
	'$read_return'.
