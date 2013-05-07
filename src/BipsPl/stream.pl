/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : stream.pl                                                       *
 * Descr.: stream selection and control management                         *
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

'$use_stream'.


current_input(Stream) :-
	set_bip_name(current_input, 1),
	'$check_stream_or_var'(Stream, S),
	'$call_c_test'('Pl_Current_Input_1'(S)).




current_output(Stream) :-
	set_bip_name(current_output, 1),
	'$check_stream_or_var'(Stream, S),
	'$call_c_test'('Pl_Current_Output_1'(S)).




'$check_stream_or_var'('$stream'(S), S) :-
	(   var(S)
	;   integer(S)
	), !.

'$check_stream_or_var'(Stream, _) :-
	'$pl_err_domain'(stream, Stream).




set_input(SorA) :-
	set_bip_name(set_input, 1),
	'$call_c'('Pl_Set_Input_1'(SorA)).




set_output(SorA) :-
	set_bip_name(set_output, 1),
	'$call_c'('Pl_Set_Output_1'(SorA)).




'$set_top_level_streams'(SorAIn, SorAOut) :-
	'$call_c'('Pl_Set_Top_Level_Streams_2'(SorAIn, SorAOut)).




'$set_debugger_streams'(SorAIn, SorAOut) :-
	'$call_c'('Pl_Set_Debugger_Streams_2'(SorAIn, SorAOut)).




          % open mask in sys_var[0]:
          %
          %   b8    b7 b6     b5    b4 b3         b2   b1         b0
          %   0/1   0/1/2     0/1   0/1/2         0/1 0/1         0/1
          %   buffering       eof_action         reposition      text
          %   b8=specified    b5=specified       b2=specified    0=text
          %   b7/6=if specif  b4/3=if specif     b1=if specified 1=binary
          %     buffering       eof action         reposition
          %     0=none          0=error            0=false
          %     1=line          1=eof_code         1=true
          %     2=block         2=reset


open(SourceSink, Mode, Stream) :-
	set_bip_name(open, 3),
	'$open'(SourceSink, Mode, Stream, []).


open(SourceSink, Mode, Stream, Options) :-
	set_bip_name(open, 4),
	'$open'(SourceSink, Mode, Stream, Options).


'$open'(SourceSink, Mode, Stream, Options) :-
	'$set_open_defaults',
	'$get_open_stm'(Stream, Stm),
	g_link('$open_aliases', LAlias),
	g_link('$open_mirrors', LMirror),
	'$get_open_options'(Options),
	g_read('$open_aliases', []), % close the list
	g_read('$open_mirrors', []), % close the list
	'$call_c'('Pl_Open_3'(SourceSink, Mode, Stm)),
	'$add_aliases_to_stream'(LAlias, Stream),
	'$add_mirrors_to_stream'(LMirror, Stream).




'$set_open_defaults' :-
	'$sys_var_write'(0, 1).




'$get_open_stm'(Stream, Stm) :-
	(   nonvar(Stream) ->
	    '$pl_err_uninstantiation'(Stream)
	;   Stream = '$stream'(Stm)
	).




'$get_open_options'(Options) :-
	'$check_list'(Options),
	'$get_open_options1'(Options).


'$get_open_options1'([]).

'$get_open_options1'([X|Options]) :-
	'$get_open_options2'(X), !,
	'$get_open_options1'(Options).


'$get_open_options2'(X) :-
	var(X),
	'$pl_err_instantiation'.

'$get_open_options2'(type(X)) :-
	nonvar(X),
	(   X = text,
	    '$sys_var_set_bit'(0, 0)
	;   X = binary,
	    '$sys_var_reset_bit'(0, 0)
	).

'$get_open_options2'(reposition(X)) :-
	nonvar(X),
	(   X = false,
	    '$sys_var_reset_bit'(0, 1)
	;   X = true,
	    '$sys_var_set_bit'(0, 1)
	),
	'$sys_var_set_bit'(0, 2).

'$get_open_options2'(eof_action(X)) :-
	nonvar(X),
	(   X = error,
	    '$sys_var_reset_bit'(0, 4),
	    '$sys_var_reset_bit'(0, 3)
	;   X = eof_code,
	    '$sys_var_reset_bit'(0, 4),
	    '$sys_var_set_bit'(0, 3)
	;   X = reset,
	    '$sys_var_set_bit'(0, 4),
	    '$sys_var_reset_bit'(0, 3)
	),
	'$sys_var_set_bit'(0, 5).

'$get_open_options2'(buffering(X)) :-
	nonvar(X),
	(   X = none,
	    '$sys_var_reset_bit'(0, 7),
	    '$sys_var_reset_bit'(0, 6)
	;   X = line,
	    '$sys_var_reset_bit'(0, 7),
	    '$sys_var_set_bit'(0, 6)
	;   X = block,
	    '$sys_var_set_bit'(0, 7),
	    '$sys_var_reset_bit'(0, 6)
	),
	'$sys_var_set_bit'(0, 8).

'$get_open_options2'(alias(X)) :-
	atom(X), !,
	(   '$call_c_test'('Pl_Test_Alias_Not_Assigned_1'(X)) ->
	    g_read('$open_aliases', [X|End]),
	    g_link('$open_aliases', End) % write new end variable
	;   '$pl_err_permission'(open, source_sink, alias(X))
	).

'$get_open_options2'(mirror(X)) :-
	nonvar(X),
	(X = '$stream'(MStm), integer(MStm) ; atom(X)),
	!,
	'$call_c'('Pl_Check_Valid_Mirror_1'(X)),
	g_read('$open_mirrors', [X|End]),
	g_link('$open_mirrors', End). % write new end variable

'$get_open_options2'(X) :-
	'$pl_err_domain'(stream_option, X).




'$add_aliases_to_stream'([], _).

'$add_aliases_to_stream'([Alias|LAlias], Stream) :-
	'$call_c'('Pl_Add_Stream_Alias_2'(Stream, Alias)),
	'$add_aliases_to_stream'(LAlias, Stream).



'$add_mirrors_to_stream'([], _).

'$add_mirrors_to_stream'([Mirror|LMirror], Stream) :-
	'$call_c'('Pl_Add_Stream_Mirror_2'(Stream, Mirror)),
	'$add_mirrors_to_stream'(LMirror, Stream).




          % close mask in sys_var[0]:
          %
          %    b0
          %    0/1
          %   force
          %   0=false
          %   1=true


close(SorA) :-
	set_bip_name(close, 1),
	'$close'(SorA, []).

close(SorA, Options) :-
	set_bip_name(close, 2),
	'$close'(SorA, Options).


'$close'(SorA, Options) :-
	'$sys_var_write'(0, 0),                                % default mask
	'$get_close_options'(Options),
	'$call_c'('Pl_Close_1'(SorA)).




'$get_close_options'(Options) :-
	'$check_list'(Options),
	'$get_close_options1'(Options).


'$get_close_options1'([]).

'$get_close_options1'([X|Options]) :-
	'$get_close_options2'(X), !,
	'$get_close_options1'(Options).


'$get_close_options2'(X) :-
	var(X),
	'$pl_err_instantiation'.

'$get_close_options2'(force(X)) :-
	nonvar(X),
	(   X = false,
	    '$sys_var_reset_bit'(0, 0)
	;   X = true,
	    '$sys_var_set_bit'(0, 0)
	).

'$get_close_options2'(X) :-
	'$pl_err_domain'(close_option, X).




add_stream_alias(SorA, Alias) :-
	set_bip_name(add_stream_alias, 2),
	'$call_c_test'('Pl_Add_Stream_Alias_2'(SorA, Alias)), !.

add_stream_alias(_, Alias) :-
	'$pl_err_permission'(add_alias, source_sink, alias(Alias)).




add_stream_mirror(SorA, Mirror) :-
	set_bip_name(add_stream_mirror, 2),
	'$call_c'('Pl_Add_Stream_Mirror_2'(SorA, Mirror)).




remove_stream_mirror(SorA, Mirror) :-
	set_bip_name(remove_stream_mirror, 2),
	'$call_c_test'('Pl_Remove_Stream_Mirror_2'(SorA, Mirror)).




set_stream_type(SorA, Type) :-
	set_bip_name(set_stream_type, 2),
	(   var(Type) ->
	    '$pl_err_instantiation'
	;   true
	),
	(   Type = text,
	    IsText = 1
	;   Type = binary,
	    IsText = 0
	), !,
	'$call_c'('Pl_Set_Stream_Type_2'(SorA, IsText)).

set_stream_type(_, Type) :-
	'$pl_err_domain'(stream_type, Type).




set_stream_eof_action(SorA, EofAction) :-
	set_bip_name(set_stream_eof_action, 2),
	(   var(EofAction) ->
	    '$pl_err_instantiation'
	;   true
	),
	(   EofAction = error,
	    Action = 0
	;   EofAction = eof_code,
	    Action = 1
	;   EofAction = reset,
	    Action = 2
	), !,
	'$call_c'('Pl_Set_Stream_Eof_Action_2'(SorA, Action)).

set_stream_eof_action(_, EofAction) :-
	'$pl_err_domain'(eof_action, EofAction).




set_stream_buffering(SorA, Buffering) :-
	set_bip_name(set_stream_buffering, 2),
	(   var(Buffering) ->
	    '$pl_err_instantiation'
	;   true
	),
	(   Buffering = none,
	    BuffMode = 0
	;   Buffering = line,
	    BuffMode = 1
	;   Buffering = block,
	    BuffMode = 2
	), !,
	'$call_c'('Pl_Set_Stream_Buffering_2'(SorA, BuffMode)).

set_stream_buffering(_, Buffering) :-
	'$pl_err_domain'(buffering_mode, Buffering).




flush_output :-
	set_bip_name(flush_output, 0),
	'$call_c'('Pl_Flush_Output_0').

flush_output(SorA) :-
	set_bip_name(flush_output, 1),
	'$call_c'('Pl_Flush_Output_1'(SorA)).




current_stream(Stream) :-
	set_bip_name(current_stream, 1),
	'$check_stream_or_var'(Stream, S),
	'$current_stream'(S).




'$current_stream'(S) :-
	'$call_c_test'('Pl_Current_Stream_1'(S)).


'$current_stream_alt' :-            % used by C code to create a choice-point
	'$call_c_test'('Pl_Current_Stream_Alt_0').




stream_property(Stream, Property) :-
	set_bip_name(stream_property, 2),
	'$check_stream_or_var'(Stream, S),
	(   nonvar(Property),
	    Property = alias(Alias),
	    atom(Alias) ->
	    '$call_c_test'('Pl_From_Alias_To_Stream_2'(Alias, S))
	;   '$check_stream_prop'(Property), !,
	    '$current_stream'(S),
	    '$stream_property1'(Property, S)
	).




'$check_stream_prop'(Property) :-
	var(Property).

'$check_stream_prop'(file_name(_)).

'$check_stream_prop'(mode(_)).

'$check_stream_prop'(input).

'$check_stream_prop'(output).

'$check_stream_prop'(alias(_)).

'$check_stream_prop'(mirror(_)).

'$check_stream_prop'(type(_)).

'$check_stream_prop'(reposition(_)).

'$check_stream_prop'(eof_action(_)).

'$check_stream_prop'(buffering(_)).

'$check_stream_prop'(end_of_stream(_)).

'$check_stream_prop'(position(_)).

'$check_stream_prop'(Property) :-
	'$pl_err_domain'(stream_property, Property).




'$stream_property1'(file_name(File), S) :-
	'$call_c_test'('Pl_Stream_Prop_File_Name_2'(File, S)).

'$stream_property1'(mode(Mode), S) :-
	'$call_c_test'('Pl_Stream_Prop_Mode_2'(Mode, S)).

'$stream_property1'(input, S) :-
	'$call_c_test'('Pl_Stream_Prop_Input_1'(S)).

'$stream_property1'(output, S) :-
	'$call_c_test'('Pl_Stream_Prop_Output_1'(S)).

'$stream_property1'(alias(Alias), S) :-
	'$current_alias'(S, Alias).

'$stream_property1'(mirror(MS), S) :-
	'$current_mirror'(S, MStm),
	MS = '$stream'(MStm).

'$stream_property1'(type(Type), S) :-
	'$call_c_test'('Pl_Stream_Prop_Type_2'(Type, S)).

'$stream_property1'(reposition(Reposition), S) :-
	'$call_c_test'('Pl_Stream_Prop_Reposition_2'(Reposition, S)).

'$stream_property1'(eof_action(EofAction), S) :-
	'$call_c_test'('Pl_Stream_Prop_Eof_Action_2'(EofAction, S)).

'$stream_property1'(buffering(Buffering), S) :-
	'$call_c_test'('Pl_Stream_Prop_Buffering_2'(Buffering, S)).

'$stream_property1'(position(Position), S) :-
	'$stream_position'('$stream'(S), Position).

'$stream_property1'(end_of_stream(EndOfStream), S) :-
	'$call_c_test'('Pl_Stream_Prop_End_Of_Stream_2'(EndOfStream, S)).





at_end_of_stream :-
	set_bip_name(at_end_of_stream, 0),
	'$call_c_test'('Pl_At_End_Of_Stream_0').

at_end_of_stream(SorA) :-
	set_bip_name(at_end_of_stream, 1),
	'$call_c_test'('Pl_At_End_Of_Stream_1'(SorA)).




current_alias(Stream, Alias) :-
	set_bip_name(current_alias, 2),
	'$check_stream_or_var'(Stream, S),
	(   atom(Alias) ->
	    '$call_c_test'('Pl_From_Alias_To_Stream_2'(Alias, S))
	;   '$current_stream'(S),
	    '$current_alias'(S, Alias)
	).




'$current_alias'(S, Alias) :-
	'$call_c_test'('Pl_Current_Alias_2'(S, Alias)).

'$current_alias_alt' :-             % used by C code to create a choice-point
	'$call_c_test'('Pl_Current_Alias_Alt_0').




current_mirror(Stream, MStream) :-
	set_bip_name(current_mirror, 2),
	'$check_stream_or_var'(Stream, S),
	'$check_stream_or_var'(MStream, MStm),
	'$current_stream'(S),
	'$current_mirror'(S, MStm).




'$current_mirror'(S, MStm) :-
	'$call_c_test'('Pl_Current_Mirror_2'(S, MStm)).

'$current_mirror_alt' :-             % used by C code to create a choice-point
	'$call_c_test'('Pl_Current_Mirror_Alt_0').




stream_position(SorA, Position) :-
	set_bip_name(stream_position, 2),
	'$stream_position'(SorA, Position).


'$stream_position'(SorA, Position) :-
	'$call_c_test'('Pl_Stream_Position_2'(SorA, Position)).




set_stream_position(SorA, Position) :-
	set_bip_name(set_stream_position, 2),
	'$call_c_test'('Pl_Set_Stream_Position_2'(SorA, Position)).




seek(SorA, Whence, Offset, NewLoc) :-
	set_bip_name(seek, 4),
	'$call_c_test'('Pl_Seek_4'(SorA, Whence, Offset, NewLoc)).




character_count(SorA, Count) :-
	set_bip_name(character_count, 2),
	'$call_c_test'('Pl_Character_Count_2'(SorA, Count)).




line_count(SorA, Count) :-
	set_bip_name(line_count, 2),
	'$call_c_test'('Pl_Line_Count_2'(SorA, Count)).




line_position(SorA, Count) :-
	set_bip_name(line_position, 2),
	'$call_c_test'('Pl_Line_Position_2'(SorA, Count)).




stream_line_column(SorA, Line, Col) :-
	set_bip_name(stream_line_column, 3),
	'$call_c_test'('Pl_Stream_Line_Column_3'(SorA, Line, Col)).




set_stream_line_column(SorA, Line, Col) :-
	set_bip_name(set_stream_line_column, 3),
	'$call_c_test'('Pl_Set_Stream_Line_Column_3'(SorA, Line, Col)).




          % term_stream operations


          % term type (open_[input/output]_xxxx_stream/2) in sys_var[0]:
          % 1=atom, 2=chars, 3=codes

open_input_atom_stream(SinkAtom, Stream) :-
	set_bip_name(open_input_atom_stream, 2),
	'$get_open_stm'(Stream, Stm),
	'$sys_var_write'(0, 1),
	'$call_c'('Pl_Open_Input_Term_Stream_2'(SinkAtom, Stm)).




open_input_chars_stream(SinkChars, Stream) :-
	set_bip_name(open_input_chars_stream, 2),
	'$get_open_stm'(Stream, Stm),
	'$sys_var_write'(0, 2),
	'$call_c'('Pl_Open_Input_Term_Stream_2'(SinkChars, Stm)).




open_input_codes_stream(SinkCodes, Stream) :-
	set_bip_name(open_input_codes_stream, 2),
	'$get_open_stm'(Stream, Stm),
	'$sys_var_write'(0, 3),
	'$call_c'('Pl_Open_Input_Term_Stream_2'(SinkCodes, Stm)).




close_input_atom_stream(SorA) :-
	set_bip_name(close_input_atom_stream, 1),
	'$sys_var_write'(0, 1),
	'$call_c'('Pl_Close_Input_Term_Stream_1'(SorA)).




close_input_chars_stream(SorA) :-
	set_bip_name(close_input_chars_stream, 1),
	'$sys_var_write'(0, 2),
	'$call_c'('Pl_Close_Input_Term_Stream_1'(SorA)).




close_input_codes_stream(SorA) :-
	set_bip_name(close_input_codes_stream, 1),
	'$sys_var_write'(0, 3),
	'$call_c'('Pl_Close_Input_Term_Stream_1'(SorA)).




open_output_atom_stream(Stream) :-
	set_bip_name(open_output_atom_stream, 1),
	'$get_open_stm'(Stream, Stm),
	'$sys_var_write'(0, 1),
	'$call_c'('Pl_Open_Output_Term_Stream_1'(Stm)).




open_output_chars_stream(Stream) :-
	set_bip_name(open_output_chars_stream, 1),
	'$get_open_stm'(Stream, Stm),
	'$sys_var_write'(0, 2),
	'$call_c'('Pl_Open_Output_Term_Stream_1'(Stm)).




open_output_codes_stream(Stream) :-
	set_bip_name(open_output_codes_stream, 1),
	'$get_open_stm'(Stream, Stm),
	'$sys_var_write'(0, 3),
	'$call_c'('Pl_Open_Output_Term_Stream_1'(Stm)).




close_output_atom_stream(SorA, SinkAtom) :-
	set_bip_name(close_output_atom_stream, 2),
	'$sys_var_write'(0, 1),
	'$call_c_test'('Pl_Close_Output_Term_Stream_2'(SorA, SinkAtom)).




close_output_chars_stream(SorA, SinkChars) :-
	set_bip_name(close_output_chars_stream, 2),
	'$sys_var_write'(0, 2),
	'$call_c_test'('Pl_Close_Output_Term_Stream_2'(SorA, SinkChars)).




close_output_codes_stream(SorA, SinkCodes) :-
	set_bip_name(close_output_codes_stream, 2),
	'$sys_var_write'(0, 3),
	'$call_c_test'('Pl_Close_Output_Term_Stream_2'(SorA, SinkCodes)).
