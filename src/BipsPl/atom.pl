/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : atom.pl                                                         *
 * Descr.: atom manipulation management                                    *
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

'$use_atom'.


atom_length(Atom, Length) :-
	set_bip_name(atom_length, 2),
	'$call_c_test'('Pl_Atom_Length_2'(Atom, Length)).




atom_concat(Atom1, Atom2, Atom3) :-
	set_bip_name(atom_concat, 3),
	'$call_c_test'('Pl_Atom_Concat_3'(Atom1, Atom2, Atom3)).

'$atom_concat_alt' :-               % used by C code to create a choice-point
	'$call_c_test'('Pl_Atom_Concat_Alt_0').



sub_atom(Atom, Before, Length, After, SubAtom) :-
	set_bip_name(sub_atom, 5),
	'$call_c_test'('Pl_Sub_Atom_5'(Atom, Before, Length, After, SubAtom)).


'$sub_atom_alt' :-                  % used by C code to create a choice-point
	'$call_c_test'('Pl_Sub_Atom_Alt_0').




atom_chars(Atom, Chars) :-
	set_bip_name(atom_chars, 2),
	'$call_c_test'('Pl_Atom_Chars_2'(Atom, Chars)).




atom_codes(Atom, Codes) :-
	set_bip_name(atom_codes, 2),
	'$call_c_test'('Pl_Atom_Codes_2'(Atom, Codes)).




number_atom(Number, Atom) :-
	set_bip_name(number_atom, 2),
	'$call_c_test'('Pl_Number_Atom_2'(Number, Atom)).




number_chars(Number, Chars) :-
	set_bip_name(number_chars, 2),
	'$call_c_test'('Pl_Number_Chars_2'(Number, Chars)).




number_codes(Number, Codes) :-
	set_bip_name(number_codes, 2),
	'$call_c_test'('Pl_Number_Codes_2'(Number, Codes)).




char_code(Char, Code) :-
	set_bip_name(char_code, 2),
	'$call_c_test'('Pl_Char_Code_2'(Char, Code)).




name(Atomic, Codes) :-
	set_bip_name(name, 2),
	'$call_c_test'('Pl_Name_2'(Atomic, Codes)).




lower_upper(Lower, Upper) :-
	set_bip_name(lower_upper, 2),
	'$call_c_test'('Pl_Lower_Upper_2'(Lower, Upper)).




current_atom(X) :-
	set_bip_name(current_atom, 1),
	'$current_atom'(X).


'$current_atom'(X) :-
	'$call_c_test'('Pl_Current_Atom_2'(X, 1)).

'$current_atom_any'(X) :-
	'$call_c_test'('Pl_Current_Atom_2'(X, 0)).


'$current_atom_alt' :-              % used by C code to create a choice-point
	'$call_c_test'('Pl_Current_Atom_Alt_0').





atom_property(Atom, Property) :-
	set_bip_name(atom_property, 2),
	'$current_atom'(Atom),
	'$atom_property1'(Atom, Property).

'$atom_property_any'(Atom, Property) :-
	'$current_atom_any'(Atom),
	'$atom_property1'(Atom, Property).




'$atom_property1'(Atom, Property) :-
	'$check_atom_prop'(Property), !,
	'$call_c'('Pl_Atom_Property_6'(Atom, PrefixOp, InfixOp, PostfixOp, NeedsQuotes, NeedsScan)),
	'$atom_property2'(Property, Atom, PrefixOp, InfixOp, PostfixOp, NeedsQuotes, NeedsScan).



'$check_atom_prop'(Property) :-
	var(Property).

'$check_atom_prop'(length(_)).

'$check_atom_prop'(hash(_)).

'$check_atom_prop'(prefix_op).

'$check_atom_prop'(infix_op).

'$check_atom_prop'(postfix_op).

'$check_atom_prop'(needs_quotes).

'$check_atom_prop'(needs_scan).

'$check_atom_prop'(Property) :-
	'$pl_err_domain'(atom_property, Property).




'$atom_property2'(length(Length), Atom, _, _, _, _, _) :-
	'$call_c_test'('Pl_Atom_Length_2'(Atom, Length)).

'$atom_property2'(hash(Hash), Atom, _, _, _, _, _) :-
	'$call_c_test'('Pl_Term_Hash_2'(Atom, Hash)).

'$atom_property2'(prefix_op, _, 1, _, _, _, _).

'$atom_property2'(infix_op, _, _, 1, _, _, _).

'$atom_property2'(postfix_op, _, _, _, 1, _, _).

'$atom_property2'(needs_quotes, _, _, _, _, 1, _).

'$atom_property2'(needs_scan, _, _, _, _, _, 1).




new_atom(X) :-
	set_bip_name(new_atom, 1),
	'$call_c_test'('Pl_New_Atom_2'(atom_, X)).

new_atom(Prefix, X) :-
	set_bip_name(new_atom, 2),
	'$call_c_test'('Pl_New_Atom_2'(Prefix, X)).

