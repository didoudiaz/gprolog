/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : char_io.pl                                                      *
 * Descr.: character input-output management                               *
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

'$use_char_io'.


get_key(Code) :-
	set_bip_name(get_key, 1),
	'$call_c_test'('Pl_Get_Key_1'(Code)).

get_key(SorA, Code) :-
	set_bip_name(get_key, 2),
	'$call_c_test'('Pl_Get_Key_2'(SorA, Code)).


get_key_no_echo(Code) :-
	set_bip_name(get_key_no_echo, 1),
	'$call_c_test'('Pl_Get_Key_No_Echo_1'(Code)).

get_key_no_echo(SorA, Code) :-
	set_bip_name(get_key_no_echo, 2),
	'$call_c_test'('Pl_Get_Key_No_Echo_2'(SorA, Code)).




          % get predicates

get_char(Char) :-
	set_bip_name(get_char, 1),
	'$call_c_test'('Pl_Get_Char_1'(Char)).

get_char(SorA, Char) :-
	set_bip_name(get_char, 2),
	'$call_c_test'('Pl_Get_Char_2'(SorA, Char)).




get_code(Code) :-
	set_bip_name(get_code, 1),
	'$call_c_test'('Pl_Get_Code_1'(Code)).

get_code(SorA, Code) :-
	set_bip_name(get_code, 2),
	'$call_c_test'('Pl_Get_Code_2'(SorA, Code)).




get_byte(Byte) :-
	set_bip_name(get_byte, 1),
	'$call_c_test'('Pl_Get_Byte_1'(Byte)).

get_byte(SorA, Byte) :-
	set_bip_name(get_byte, 2),
	'$call_c_test'('Pl_Get_Byte_2'(SorA, Byte)).





          % unget predicates

unget_char(Char) :-
	set_bip_name(unget_char, 1),
	'$call_c'('Pl_Unget_Char_1'(Char)).

unget_char(SorA, Char) :-
	set_bip_name(unget_char, 2),
	'$call_c'('Pl_Unget_Char_2'(SorA, Char)).




unget_code(Code) :-
	set_bip_name(unget_code, 1),
	'$call_c'('Pl_Unget_Code_1'(Code)).

unget_code(SorA, Code) :-
	set_bip_name(unget_code, 2),
	'$call_c'('Pl_Unget_Code_2'(SorA, Code)).




unget_byte(Byte) :-
	set_bip_name(unget_byte, 1),
	'$call_c'('Pl_Unget_Byte_1'(Byte)).

unget_byte(SorA, Byte) :-
	set_bip_name(unget_byte, 2),
	'$call_c'('Pl_Unget_Byte_2'(SorA, Byte)).




          % peek predicates

peek_char(Char) :-
	set_bip_name(peek_char, 1),
	'$call_c_test'('Pl_Peek_Char_1'(Char)).

peek_char(SorA, Char) :-
	set_bip_name(peek_char, 2),
	'$call_c_test'('Pl_Peek_Char_2'(SorA, Char)).




peek_code(Code) :-
	set_bip_name(peek_code, 1),
	'$call_c_test'('Pl_Peek_Code_1'(Code)).

peek_code(SorA, Code) :-
	set_bip_name(peek_code, 2),
	'$call_c_test'('Pl_Peek_Code_2'(SorA, Code)).




peek_byte(Byte) :-
	set_bip_name(peek_byte, 1),
	'$call_c_test'('Pl_Peek_Byte_1'(Byte)).

peek_byte(SorA, Byte) :-
	set_bip_name(peek_byte, 2),
	'$call_c_test'('Pl_Peek_Byte_2'(SorA, Byte)).




          % put predicates

put_char(Char) :-
	set_bip_name(put_char, 1),
	'$call_c'('Pl_Put_Char_1'(Char)).

put_char(SorA, Char) :-
	set_bip_name(put_char, 2),
	'$call_c'('Pl_Put_Char_2'(SorA, Char)).




put_code(Code) :-
	set_bip_name(put_code, 1),
	'$call_c'('Pl_Put_Code_1'(Code)).

put_code(SorA, Code) :-
	set_bip_name(put_code, 2),
	'$call_c'('Pl_Put_Code_2'(SorA, Code)).




put_byte(Byte) :-
	set_bip_name(put_byte, 1),
	'$call_c'('Pl_Put_Byte_1'(Byte)).

put_byte(SorA, Byte) :-
	set_bip_name(put_byte, 2),
	'$call_c'('Pl_Put_Byte_2'(SorA, Byte)).
