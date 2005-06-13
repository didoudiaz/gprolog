/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : const_io.pl                                                     * 
 * Descr.: input/output from/to constant term management                   * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2005 Daniel Diaz                                     * 
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
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               * 
 *-------------------------------------------------------------------------*/

/* $Id$ */

:-	built_in.

'$use_const_io'.




write_to_atom(Atom, Term) :-
	set_bip_name(write_to_atom, 2),
	'$call_c_test'('Write_To_Atom_2'(Atom, Term)).

write_to_chars(Chars, Term) :-
	set_bip_name(write_to_chars, 2),
	'$call_c_test'('Write_To_Chars_2'(Chars, Term)).

write_to_codes(Codes, Term) :-
	set_bip_name(write_to_codes, 2),
	'$call_c_test'('Write_To_Codes_2'(Codes, Term)).




writeq_to_atom(Atom, Term) :-
	set_bip_name(writeq_to_atom, 2),
	'$call_c_test'('Writeq_To_Atom_2'(Atom, Term)).

writeq_to_chars(Chars, Term) :-
	set_bip_name(writeq_to_chars, 2),
	'$call_c_test'('Writeq_To_Chars_2'(Chars, Term)).

writeq_to_codes(Codes, Term) :-
	set_bip_name(writeq_to_codes, 2),
	'$call_c_test'('Writeq_To_Codes_2'(Codes, Term)).




write_canonical_to_atom(Atom, Term) :-
	set_bip_name(write_canonical_to_atom, 2),
	'$call_c_test'('Write_Canonical_To_Atom_2'(Atom, Term)).

write_canonical_to_chars(Chars, Term) :-
	set_bip_name(write_canonical_to_chars, 2),
	'$call_c_test'('Write_Canonical_To_Chars_2'(Chars, Term)).

write_canonical_to_codes(Codes, Term) :-
	set_bip_name(write_canonical_to_codes, 2),
	'$call_c_test'('Write_Canonical_To_Codes_2'(Codes, Term)).




display_to_atom(Atom, Term) :-
	set_bip_name(display_to_atom, 2),
	'$call_c_test'('Display_To_Atom_2'(Atom, Term)).

display_to_chars(Chars, Term) :-
	set_bip_name(display_to_chars, 2),
	'$call_c_test'('Display_To_Chars_2'(Chars, Term)).

display_to_codes(Codes, Term) :-
	set_bip_name(display_to_codes, 2),
	'$call_c_test'('Display_To_Codes_2'(Codes, Term)).




print_to_atom(Atom, Term) :-
	set_bip_name(print_to_atom, 2),
	'$call_c_test'('Print_To_Atom_2'(Atom, Term)).

print_to_chars(Chars, Term) :-
	set_bip_name(print_to_chars, 2),
	'$call_c_test'('Print_To_Chars_2'(Chars, Term)).

print_to_codes(Codes, Term) :-
	set_bip_name(print_to_codes, 2),
	'$call_c_test'('Print_To_Codes_2'(Codes, Term)).




write_term_to_atom(Atom, Term, Options) :-
	set_bip_name(write_term_to_atom, 3),
	'$set_write_defaults',
	'$get_write_options'(Options),
	'$call_c'('Write_Term_To_Atom_2'(Atom, Term)).

write_term_to_chars(Chars, Term, Options) :-
	set_bip_name(write_term_to_chars, 3),
	'$set_write_defaults',
	'$get_write_options'(Options),
	'$call_c'('Write_Term_To_Chars_2'(Chars, Term)).

write_term_to_codes(Codes, Term, Options) :-
	set_bip_name(write_term_to_codes, 3),
	'$set_write_defaults',
	'$get_write_options'(Options),
	'$call_c'('Write_Term_To_Codes_2'(Codes, Term)).




format_to_atom(Atom, Format, Args) :-
	set_bip_name(format_to_atom, 3),
	'$call_c_test'('Format_To_Atom_3'(Atom, Format, Args)).

format_to_chars(Chars, Format, Args) :-
	set_bip_name(format_to_chars, 3),
	'$call_c_test'('Format_To_Chars_3'(Chars, Format, Args)).

format_to_codes(Codes, Format, Args) :-
	set_bip_name(format_to_codes, 3),
	'$call_c_test'('Format_To_Codes_3'(Codes, Format, Args)).




read_from_atom(Atom, Term) :-
	set_bip_name(read_from_atom, 2),
	'$set_read_defaults',
	'$call_c_test'('Read_From_Atom_2'(Atom, Term)).

read_from_chars(Chars, Term) :-
	set_bip_name(read_from_chars, 2),
	'$set_read_defaults',
	'$call_c_test'('Read_From_Chars_2'(Chars, Term)).

read_from_codes(Codes, Term) :-
	set_bip_name(read_from_codes, 2),
	'$set_read_defaults',
	'$call_c_test'('Read_From_Codes_2'(Codes, Term)).




read_term_from_atom(Atom, Term, Options) :-
	set_bip_name(read_term_from_atom, 3),
	'$set_read_defaults',
	'$get_read_options'(Options, Vars, VarNames, SinglNames),
	'$call_c_test'('Read_Term_From_Atom_5'(Atom, Term, Vars, VarNames, SinglNames)).

read_term_from_chars(Chars, Term, Options) :-
	set_bip_name(read_term_from_chars, 3),
	'$set_read_defaults',
	'$get_read_options'(Options, Vars, VarNames, SinglNames),
	'$call_c_test'('Read_Term_From_Chars_5'(Chars, Term, Vars, VarNames, SinglNames)).

read_term_from_codes(Codes, Term, Options) :-
	set_bip_name(read_term_from_codes, 3),
	'$set_read_defaults',
	'$get_read_options'(Options, Vars, VarNames, SinglNames),
	'$call_c_test'('Read_Term_From_Codes_5'(Codes, Term, Vars, VarNames, SinglNames)).




read_token_from_atom(Atom, Token) :-
	set_bip_name(read_token_from_atom, 2),
	'$set_read_defaults',
	'$call_c_test'('Read_Token_From_Atom_2'(Atom, Token)).

read_token_from_chars(Chars, Token) :-
	set_bip_name(read_token_from_chars, 2),
	'$set_read_defaults',
	'$call_c_test'('Read_Token_From_Chars_2'(Chars, Token)).

read_token_from_codes(Codes, Token) :-
	set_bip_name(read_token_from_codes, 2),
	'$set_read_defaults',
	'$call_c_test'('Read_Token_From_Codes_2'(Codes, Token)).
