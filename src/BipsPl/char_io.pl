/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog buit-in predicates                                       */
/* File  : char_io.pl                                                      */
/* Descr.: character input-output management                               */
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

:- built_in.

'$use_char_io'.


get_code_no_echo(Code):-
	set_bip_name(get_code_no_echo,1),
	'$call_c_test'('Get_Code_No_Echo_1'(Code)).

get_code_no_echo(SorA,Code):-
	set_bip_name(get_code_no_echo,2),
	'$call_c_test'('Get_Code_No_Echo_2'(SorA,Code)).




          /* get predicates */

get_char(Char):-
	set_bip_name(get_char,1),
	'$call_c_test'('Get_Char_1'(Char)).

get_char(SorA,Char):-
	set_bip_name(get_char,2),
	'$call_c_test'('Get_Char_2'(SorA,Char)).




get_code(Code):-
	set_bip_name(get_code,1),
	'$call_c_test'('Get_Code_1'(Code)).

get_code(SorA,Code):-
	set_bip_name(get_code,2),
	'$call_c_test'('Get_Code_2'(SorA,Code)).




get_byte(Byte):-
	set_bip_name(get_byte,1),
	'$call_c_test'('Get_Byte_1'(Byte)).

get_byte(SorA,Byte):-
	set_bip_name(get_byte,2),
	'$call_c_test'('Get_Byte_2'(SorA,Byte)).





          /* unget predicates */

unget_char(Char):-
	set_bip_name(unget_char,1),
	'$call_c'('Unget_Char_1'(Char)).

unget_char(SorA,Char):-
	set_bip_name(unget_char,2),
	'$call_c'('Unget_Char_2'(SorA,Char)).




unget_code(Code):-
	set_bip_name(unget_code,1),
	'$call_c'('Unget_Code_1'(Code)).

unget_code(SorA,Code):-
	set_bip_name(unget_code,2),
	'$call_c'('Unget_Code_2'(SorA,Code)).




unget_byte(Byte):-
	set_bip_name(unget_byte,1),
	'$call_c'('Unget_Byte_1'(Byte)).

unget_byte(SorA,Byte):-
	set_bip_name(unget_byte,2),
	'$call_c'('Unget_Byte_2'(SorA,Byte)).




          /* peek predicates */

peek_char(Char):-
	set_bip_name(peek_char,1),
	'$call_c_test'('Peek_Char_1'(Char)).

peek_char(SorA,Char):-
	set_bip_name(peek_char,2),
	'$call_c_test'('Peek_Char_2'(SorA,Char)).




peek_code(Code):-
	set_bip_name(peek_code,1),
	'$call_c_test'('Peek_Code_1'(Code)).

peek_code(SorA,Code):-
	set_bip_name(peek_code,2),
	'$call_c_test'('Peek_Code_2'(SorA,Code)).




peek_byte(Byte):-
	set_bip_name(peek_byte,1),
	'$call_c_test'('Peek_Byte_1'(Byte)).

peek_byte(SorA,Byte):-
	set_bip_name(peek_byte,2),
	'$call_c_test'('Peek_Byte_2'(SorA,Byte)).




          /* put predicates */

put_char(Char):-
	set_bip_name(put_char,1),
	'$call_c'('Put_Char_1'(Char)).

put_char(SorA,Char):-
	set_bip_name(put_char,2),
	'$call_c'('Put_Char_2'(SorA,Char)).




put_code(Code):-
	set_bip_name(put_code,1),
	'$call_c'('Put_Code_1'(Code)).

put_code(SorA,Code):-
	set_bip_name(put_code,2),
	'$call_c'('Put_Code_2'(SorA,Code)).




put_byte(Byte):-
	set_bip_name(put_byte,1),
	'$call_c'('Put_Byte_1'(Byte)).

put_byte(SorA,Byte):-
	set_bip_name(put_byte,2),
	'$call_c'('Put_Byte_2'(SorA,Byte)).
