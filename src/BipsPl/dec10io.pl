/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : dec10io.pl                                                      *
 * Descr.: DEC-10 I/O compatibility library                                *
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

'$use_dec10io'.



'$find_existing_stream'(user, '$stream'(0), input).

'$find_existing_stream'(user_input, '$stream'(0), input).

'$find_existing_stream'(user, '$stream'(1), output).

'$find_existing_stream'(user_output, '$stream'(1), output).

'$find_existing_stream'(File, Stream, Mode) :-
	clause('$dec10_stream'(File, Stream, Mode), _), !,
	(   current_stream(Stream),                 % test if it still exists
	    stream_property(Stream, file_name(File)),   % (with correct name)
	    stream_property(Stream, Mode)                % (and correct mode)
	                                  ->
	    true
	;   retract('$dec10_stream'(File, Stream, Mode)),       % no, retract
	    fail
	).

'$find_existing_stream'(Stream, Stream, Mode) :-
	Stream = '$stream'(S),                       % test if it is a stream
	integer(S),
	current_stream(Stream),
	stream_property(Stream, Mode).




see(File) :-
	set_bip_name(see, 1),
	nonvar(File), !,
	(   '$find_existing_stream'(File, Stream, input)
	;   set_bip_name(see, 1),
	    '$open'(File, read, Stream, []),
	    assertz('$dec10_stream'(File, Stream, input))
	), !,
	set_input(Stream).

see(_) :-
	'$pl_err_instantiation'.




seeing(File) :-
	set_bip_name(seeing, 1),
	current_input(Stream),
	'$find_existing_stream'(File, Stream, input), !.




seen :-
	set_bip_name(seen, 0),
	current_input(Stream),
	close(Stream),                      % before find_existing to retract
	(   '$find_existing_stream'(_, Stream, input)
	;   true
	), !.



tell(File) :-
	set_bip_name(tell, 1),
	nonvar(File), !,
	(   '$find_existing_stream'(File, Stream, output)
	;   set_bip_name(tell, 1),
	    '$open'(File, write, Stream, []),
	    assertz('$dec10_stream'(File, Stream, output))
	), !,
	set_output(Stream).

tell(_) :-
	'$pl_err_instantiation'.




telling(File) :-
	set_bip_name(telling, 1),
	current_output(Stream),
	'$find_existing_stream'(File, Stream, output), !.




told :-
	set_bip_name(told, 0),
	current_output(Stream),
	close(Stream),                      % before find_existing to retract
	(   '$find_existing_stream'(_, Stream, output)
	;   true
	), !.


append(File) :-
	set_bip_name(append, 1),
	nonvar(File), !,
	(   '$find_existing_stream'(File, Stream, output)
	;   set_bip_name(append, 1),
	    '$open'(File, append, Stream, []),
	    assertz('$dec10_stream'(File, Stream, output))
	), !,
	set_output(Stream).

append(_) :-
	'$pl_err_instantiation'.




get0(X) :-
	set_bip_name(get0, 1),
	'$call_c_test'('Pl_Get_Code_1'(X)).



get(X) :-
	set_bip_name(get, 1),
	'$check_in_character_code'(X),
	'$call_c_test'('Pl_Get_Code_1'(X0)),
	(   X0 =< 32 ->
	    get(X)
	;   X = X0
	).

put(X) :-
	set_bip_name(put, 1),
	'$call_c'('Pl_Put_Code_1'(X)).


skip(X) :-
	set_bip_name(skip, 1),
	'$check_in_character_code'(X),
	repeat,
	'$call_c_test'('Pl_Get_Code_1'(X0)),
	X0 = X, !.




'$check_in_character_code'(X) :-
	var(X), !.

'$check_in_character_code'(X) :-
	integer(X), !,
	(   X >= -1,
	    X =< 255 ->
	    true
	;   '$pl_err_representation'(in_character_code)
	).

'$check_in_character_code'(X) :-
	'$pl_err_type'(integer, X).


tab(Exp) :-
	set_bip_name(tab, 1),
	'$arith_eval'(Exp, N),
	for(_, 1, N),
	put_char(' '),
	fail.

tab(_).
