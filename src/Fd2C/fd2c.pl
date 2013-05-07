/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint definition file to C code compiler                *
 * File  : fd2c.pl                                                         *
 * Descr.: main file                                                       *
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


fd2c(Args) :-
	catch(fd2c1(Args), Err, exception(Err)).




fd2c1(Args) :-
	cmd_line_args(Args, FdFile0, CFile0),
	fd_file(FdFile0, FdFile),
	c_file(CFile0, FdFile, CFile),
	open(FdFile, read, _, [alias(stream_fd)]),
	open(CFile, write, _, [alias(stream_c)]),
	emit_code_init(FdFile),
	file_to_token_lst(LToken),
	close(stream_fd),
	parse_and_emit(LToken),
	close(stream_c).





parse_and_emit([]).

parse_and_emit(LToken) :-
	(   LToken = [] ->
	    true
	;   parse_user_cstr(Cstr, LToken, LToken1),
	    emit_user_cstr(Cstr),
	    parse_and_emit(LToken1)
	).




fd_file(FdFile, FdFile1) :-
	decompose_file_name(FdFile, _, _, Suffix),
	(   Suffix \== '' ->
	    FdFile1 = FdFile
	;   atom_concat(FdFile, '.fd', FdFile1)
	).




c_file('', FdFile, CFile) :-
	!,
	decompose_file_name(FdFile, _, Prefix, _),
	atom_concat(Prefix, '.c', CFile).

c_file(CFile, _, CFile).





          % Command-line options reading

cmd_line_args(Args, FdFile, CFile) :-
	g_assign(fdfile, ''),
	g_assign(cfile, ''),
	cmd_line_args(Args),
	g_read(fdfile, FdFile),
	(   FdFile = '' ->
	    write('no input file'),
	    nl,
	    abort
	;   true
	),
	g_read(cfile, CFile).




cmd_line_args([]).

cmd_line_args([Arg|LArg]) :-
	cmd_line_arg1(Arg, LArg, LArg1), !,
	cmd_line_args(LArg1).


cmd_line_arg1('-o', LArg, LArg1) :-
	cmd_line_arg1('--output', LArg, LArg1).

cmd_line_arg1('--output', LArg, LArg1) :-
	(   LArg = [CFile|LArg1],
	    sub_atom(CFile, 0, 1, _, Prefix),
	    Prefix \== (-)
	;   format('FILE missing after --output option~n', []),
	    abort
	),
	g_read(cfile, CFile0),
	(   CFile0 = '' ->
	    true
	;   format('output file already specified (~a)~n', [CFile0]),
	    abort
	),
	g_assign(cfile, CFile).

cmd_line_arg1('--version', LArg, LArg) :-
	display_copying,
	stop.

cmd_line_arg1('-h', LArg, LArg1) :-
	cmd_line_arg1('--help', LArg, LArg1).

cmd_line_arg1('--help', LArg, LArg) :-
	(   h(L),
	    write(L),
	    nl,
	    fail
	;   nl,
	    write('Report bugs to bug-prolog@gnu.org.'),
	    nl,
	    stop
	).


cmd_line_arg1(Arg, _, _) :-
	sub_atom(Arg, 0, 1, _, -),
	format('unknown option ~a - try fd2c --help~n', [Arg]),
	abort.

cmd_line_arg1(FdFile, LArg, LArg) :-
	g_read(fdfile, FdFile0),
	(   FdFile0 = '' ->
	    true
	;   format('input file already specified (~a)~n', [FdFile0]),
	    abort
	),
	g_assign(fdfile, FdFile).




          % Copying

display_copying :-
	current_prolog_flag(prolog_name, Name),
	current_prolog_flag(prolog_version, Version),
	current_prolog_flag(prolog_copyright, Copyright),
	format('FD Constraints to C Compiler (~a) ~a~n', [Name, Version]),
	format('By Daniel Diaz~n', []),
	write(Copyright),
	nl,
	format('~a comes with ABSOLUTELY NO WARRANTY.~n', [Name]),
	format('You may redistribute copies of ~a~n', [Name]),
	format('under the terms of the GNU Lesser General Public License.~n', []),
	format('For more information about these matters, see the files named COPYING.~n', []).




          % Help

h('Usage: fd2c [OPTION...] FILE').
h('').
h('Options:').
h('  -o FILE, --output FILE      set output file name').
h('  --help                      print this help and exit').
h('  --version                   print version number and exit').
h('').
h('''user'' can be given as FILE for the standard input/output').




          % Exception recovery

exception(error(syntax_error(_), _)) :-
	!,
	syntax_error_info(_, Line, Char, Msg),
	error('~d syntax error: ~a (char:~d)', [Line, Msg, Char]).

exception(error(existence_error(source_sink, File), _)) :-
	!,
	error('cannot open file ~w - does not exist', [File]).

exception(error(permission_error(open, source_sink, File), _)) :-
	!,
	error('cannot open file ~w - permission error', [File]).

exception(Err) :-
	error('exception raised: ~w', [Err]).




error(Msg, Args) :-
	g_read(fdfile, FdFile),
	format(user_output, 'error: ~a:', [FdFile]),
	format(user_output, Msg, Args),
	nl(user_output),
	abort.




warn(Msg, Args) :-
	g_read(fdfile, FdFile),
	format(user_output, 'warning: ~a:', [FdFile]),
	format(user_output, Msg, Args),
	nl(user_output).




          % Starting directive

go :-
	argument_list(L),
	fd2c(L).

:-	initialization(go).
