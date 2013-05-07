/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : write.pl                                                        *
 * Descr.: term output (write/1 and friends) management                    *
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

'$use_write'.


/* warning: if you change this file check also definitions in const_io.pl */




write(Term) :-
	set_bip_name(write, 1),
	'$call_c'('Pl_Write_1'(Term)).

write(SorA, Term) :-
	set_bip_name(write, 2),
	'$call_c'('Pl_Write_2'(SorA, Term)).




writeq(Term) :-
	set_bip_name(writeq, 1),
	'$call_c'('Pl_Writeq_1'(Term)).

writeq(SorA, Term) :-
	set_bip_name(writeq, 2),
	'$call_c'('Pl_Writeq_2'(SorA, Term)).




write_canonical(Term) :-
	set_bip_name(write_canonical, 1),
	'$call_c'('Pl_Write_Canonical_1'(Term)).

write_canonical(SorA, Term) :-
	set_bip_name(write_canonical, 2),
	'$call_c'('Pl_Write_Canonical_2'(SorA, Term)).




display(Term) :-
	set_bip_name(display, 1),
	'$call_c'('Pl_Display_1'(Term)).

display(SorA, Term) :-
	set_bip_name(display, 2),
	'$call_c'('Pl_Display_2'(SorA, Term)).




          % see file print.pl for the definition of print/1-2




          % option mask in sys_var[0]: (see write_supp.h)
          %
          %    b6          b5         b4        b3         b2           b1         b0
          %    0/1         0/1        0/1       0/1        0/1          0/1        0/1
          % var_names   portrayed  space_args  namevars  numbervars  ignore_ops  quoted
          %  0=false     0=false    0=false    0=false    0=false      0=false   0=false
          %  1=true      1=true     1=true     1=true     1=true       1=true    1=true
          %
          % max_depth in sys_var[1]
          % priority in sys_var[2]


write_term(Term, Options) :-
	set_bip_name(write_term, 2),
	'$set_write_defaults',
	'$get_write_options'(Options),
	'$call_c'('Pl_Write_Term_1'(Term)),
	fail.

write_term(_, _).
	

write_term(SorA, Term, Options) :-
	set_bip_name(write_term, 3),
	'$set_write_defaults',
	'$get_write_options'(Options),
	'$call_c'('Pl_Write_Term_2'(SorA, Term)),
	fail.

write_term(_, _, _).



'$set_write_defaults' :-
	'$sys_var_write'(0, 0),                               % default mask
	'$sys_var_write'(1, -1),
	'$sys_var_write'(2, 1200),
	'$sys_var_write'(3, 0).




'$get_write_options'(Options) :-
	'$check_list'(Options),
	'$get_write_options1'(Options),
	(   '$sys_var_get_bit'(0, 6, 1) -> % variable_names ==> namevars
	    '$sys_var_set_bit'(0, 3)
	;
	    true
	).
	    


'$get_write_options1'([]).

'$get_write_options1'([X|Options]) :-
	'$get_write_options2'(X), !,
	'$get_write_options1'(Options).


'$get_write_options2'(X) :-
	var(X),
	'$pl_err_instantiation'.

'$get_write_options2'(quoted(X)) :-
	nonvar(X),
	(   X = false,
	    '$sys_var_reset_bit'(0, 0)
	;   X = true,
	    '$sys_var_set_bit'(0, 0)
	).

'$get_write_options2'(ignore_ops(X)) :-
	nonvar(X),
	(   X = false,
	    '$sys_var_reset_bit'(0, 1)
	;   X = true,
	    '$sys_var_set_bit'(0, 1)
	).

'$get_write_options2'(numbervars(X)) :-
	nonvar(X),
	(   X = false,
	    '$sys_var_reset_bit'(0, 2)
	;   X = true,
	    '$sys_var_set_bit'(0, 2)
	).

'$get_write_options2'(namevars(X)) :-
	nonvar(X),
	(   X = false,
	    '$sys_var_reset_bit'(0, 3)
	;   X = true,
	    '$sys_var_set_bit'(0, 3)
	).

'$get_write_options2'('$above'(X)) :- % "above" choice-point for numbervars/namevars
	integer(X),
	'$sys_var_write'(3, X).

'$get_write_options2'(space_args(X)) :-
	nonvar(X),
	(   X = false,
	    '$sys_var_reset_bit'(0, 4)
	;   X = true,
	    '$sys_var_set_bit'(0, 4)
	).

'$get_write_options2'(portrayed(X)) :-
	nonvar(X),
	(   X = false,
	    '$sys_var_reset_bit'(0, 5)
	;   X = true,
	    '$sys_var_set_bit'(0, 5)
	).

'$get_write_options2'(variable_names(VarNames)) :-
	'$sys_var_set_bit'(0, 6),
	'$name_variables'(VarNames).

'$get_write_options2'(max_depth(X)) :-
	integer(X),
	'$sys_var_write'(1, X).

'$get_write_options2'(priority(X)) :-
	integer(X),
	'$sys_var_write'(2, X).

'$get_write_options2'(X) :-
	'$pl_err_domain'(write_option, X).



'$name_variables'([]).

'$name_variables'([Name = Var|VarNames]) :-
	('$is_valid_var_name'(Name), Var = '$VARNAME'(Name), ! ; true),
	'$name_variables'(VarNames).



'$is_valid_var_name'(Name) :-
	'$call_c_test'('Pl_Is_Valid_Var_Name_1'(Name)).



nl :-
	set_bip_name(nl, 0),
	'$call_c'('Pl_Nl_0').

nl(SorA) :-
	set_bip_name(nl, 1),
	'$call_c'('Pl_Nl_1'(SorA)).
