/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : read.pl                                                         *
 * Descr.: term input (read/1 and friends) management                      *
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

'$use_read'.


/* warning: if you change this file check also definitions in const_io.pl */




read(Term) :-
	set_bip_name(read, 1),
	'$set_read_defaults',
	'$call_c_test'('Pl_Read_1'(Term)).

read(SorA, Term) :-
	set_bip_name(read, 2),
	'$set_read_defaults',
	'$call_c_test'('Pl_Read_2'(SorA, Term)).


          % option mask in sys_var[0]:
          %
          %    b3            b2          b1         b0
          %    0/1           0/1         0/1        0/1
          % end_of_term   singletons   var_names   vars
          %   0=dot         0=false     0=false    0=false
          %   1=eof         1=true      1=true     1=true
          %
          % syntax error action in sys_var[1]:
          %
          %   -1=not specified
          %    0=error    raise an error (iso)
          %    1=waning   display a message and fail
          %    2=fail     quietly fail


read_term(Term, Options) :-
	set_bip_name(read_term, 2),
	'$set_read_defaults',
	'$get_read_options'(Options, Vars, VarNames, SinglNames),
	'$call_c_test'('Pl_Read_Term_4'(Term, Vars, VarNames, SinglNames)).


read_term(SorA, Term, Options) :-
	set_bip_name(read_term, 3),
	'$read_term'(SorA, Term, Options).

'$read_term'(SorA, Term, Options) :-
	'$set_read_defaults',
	'$get_read_options'(Options, Vars, VarNames, SinglNames),
	'$call_c_test'('Pl_Read_Term_5'(SorA, Term, Vars, VarNames, SinglNames)).




'$set_read_defaults' :-		%if modified, modified also const_io_c.c
	'$sys_var_write'(0, 0),                                % default mask
	'$sys_var_write'(1, -1).




'$get_read_options'(Options, Vars, VarNames, SinglNames) :-
	'$check_list'(Options),
	g_assign('$read_variables', []),
	g_assign('$read_variable_names', []),
	g_assign('$read_singletons', []),
	'$get_read_options1'(Options),
	g_read('$read_variables', Vars),
	g_read('$read_variable_names', VarNames),
	g_read('$read_singletons', SinglNames).


'$get_read_options1'([]).

'$get_read_options1'([X|Options]) :-
	'$get_read_options2'(X), !,
	'$get_read_options1'(Options).


'$get_read_options2'(X) :-
	var(X),
	'$pl_err_instantiation'.

'$get_read_options2'(variables(Vars)) :-
	g_link('$read_variables', Vars),
	'$sys_var_set_bit'(0, 0).

'$get_read_options2'(variable_names(VarNames)) :-
	g_link('$read_variable_names', VarNames),
	'$sys_var_set_bit'(0, 1).

'$get_read_options2'(singletons(SinglNames)) :-
	g_link('$read_singletons', SinglNames),
	'$sys_var_set_bit'(0, 2).

'$get_read_options2'(syntax_error(X)) :-
	nonvar(X),
	(   X = error,
	    '$sys_var_write'(1, 0)             % same order as in flag_supp.h
	;   X = warning,
	    '$sys_var_write'(1, 1)
	;   X = fail,
	    '$sys_var_write'(1, 2)
	).

'$get_read_options2'(end_of_term(X)) :-
	nonvar(X),                            % same order as in parse_supp.h
	(   X = dot,
	    '$sys_var_reset_bit'(0, 3)
	;   X = eof,
	    '$sys_var_set_bit'(0, 3)
	).

'$get_read_options2'(X) :-
	'$pl_err_domain'(read_option, X).




read_atom(Atom) :-
	set_bip_name(read_atom, 1),
	'$set_read_defaults',
	'$call_c_test'('Pl_Read_Atom_1'(Atom)).


read_atom(SorA, Atom) :-
	set_bip_name(read_atom, 2),
	'$set_read_defaults',
	'$call_c_test'('Pl_Read_Atom_2'(SorA, Atom)).




read_integer(Integer) :-
	set_bip_name(read_integer, 1),
	'$set_read_defaults',
	'$call_c_test'('Pl_Read_Integer_1'(Integer)).


read_integer(SorA, Integer) :-
	set_bip_name(read_integer, 2),
	'$set_read_defaults',
	'$call_c_test'('Pl_Read_Integer_2'(SorA, Integer)).




read_number(Number) :-
	set_bip_name(read_number, 1),
	'$set_read_defaults',
	'$call_c_test'('Pl_Read_Number_1'(Number)).


read_number(SorA, Number) :-
	set_bip_name(read_number, 2),
	'$set_read_defaults',
	'$call_c_test'('Pl_Read_Number_2'(SorA, Number)).




read_token(Token) :-
	set_bip_name(read_token, 1),
	'$call_c_test'('Pl_Read_Token_1'(Token)).

read_token(SorA, Token) :-
	set_bip_name(read_token, 2),
	'$call_c_test'('Pl_Read_Token_2'(SorA, Token)).



last_read_start_line_column(Line, Col) :-
	set_bip_name(last_read_start_line_column, 2),
	'$call_c_test'('Pl_Last_Read_Start_Line_Column_2'(Line, Col)).




char_conversion(InChar, OutChar) :-
	set_bip_name(char_conversion, 2),
	'$call_c'('Pl_Char_Conversion_2'(InChar, OutChar)).




current_char_conversion(InChar, OutChar) :-
	set_bip_name(current_char_conversion, 2),
	'$call_c_test'('Pl_Current_Char_Conversion_2'(InChar, OutChar)).




'$current_char_conversion_alt' :-   % used by C code to create a choice-point
	'$call_c_test'('Pl_Current_Char_Conversion_Alt_0').
