/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : pretty.pl                                                       *
 * Descr.: pretty print clause management                                  *
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

'$use_pretty'.


portray_clause(Term) :-
	'$portray_clause'(Term, 1, AboveB),
	'$call_c'('Pl_Portray_Clause_2'(Term, AboveB)),
	fail.

portray_clause(_).




portray_clause(SorA, Term) :-
	'$portray_clause'(Term, 2, AboveB),
	'$call_c'('Pl_Portray_Clause_3'(SorA, Term, AboveB)),
	fail.

portray_clause(_, _).




'$portray_clause'(Term, Arity, AboveB) :- % create choice point for '$above'/1 write option
        '$get_current_B'(AboveB),	
	set_bip_name(portray_clause, Arity),
	(   var(Term) ->
	    '$pl_err_instantiation'
	;   true
	),
	(   callable(Term) ->
	    true
	;   '$pl_err_type'(callable, Term)
	),
	name_singleton_vars(Term),
	bind_variables(Term, [exclude([Term])]),
	set_bip_name(portray_clause, Arity).

'$portray_clause'(_, _, _) :-
	fail.



name_singleton_vars(Term) :-
	set_bip_name(name_singleton_vars, 1),
	'$call_c'('Pl_Name_Singleton_Vars_1'(Term)).




name_query_vars(QueryVars, RestVars) :-
	set_bip_name(name_query_vars, 2),
	'$call_c_test'('Pl_Name_Query_Vars_2'(QueryVars, RestVars)).




          % sys_var[0]:0 if numbervars bindings, 1 if namevars bindings

bind_variables(Term, Options) :-
	set_bip_name(bind_variables, 2),
	'$set_bind_variables_defaults',
	'$get_bind_variables_options'(Options, Exclude, From, Next),
	'$bind_variables'(Term, Exclude, From, Next).


'$bind_variables'(Term, Exclude, From, Next) :-
	'$call_c_test'('Pl_Bind_Variables_4'(Term, Exclude, From, Next)).




'$set_bind_variables_defaults' :-
	'$sys_var_write'(0, 0).




'$get_bind_variables_options'(Options, Exclude, From, Next) :-
	'$check_list'(Options),
	g_assign('$bind_exclude', []),
	g_assign('$bind_from', 0),
	g_assign('$bind_next', _),
	'$get_bind_variables_options1'(Options),
	g_read('$bind_exclude', Exclude),
	g_read('$bind_from', From),
	g_read('$bind_next', Next).


'$get_bind_variables_options1'([]).

'$get_bind_variables_options1'([X|Options]) :-
	'$get_bind_variables_options2'(X), !,
	'$get_bind_variables_options1'(Options).


'$get_bind_variables_options2'(X) :-
	var(X),
	'$pl_err_instantiation'.

'$get_bind_variables_options2'(exclude(Exclude)) :-
	g_link('$bind_exclude', Exclude).

'$get_bind_variables_options2'(from(From)) :-
	g_link('$bind_from', From).

'$get_bind_variables_options2'(next(Next)) :-
	g_link('$bind_next', Next).

'$get_bind_variables_options2'(numbervars) :-
	'$sys_var_write'(0, 0).

'$get_bind_variables_options2'(namevars) :-
	'$sys_var_write'(0, 1).

'$get_bind_variables_options2'(X) :-
	'$pl_err_domain'(var_binding_option, X).




numbervars(Term) :-
	set_bip_name(numbervars, 1),
	'$set_bind_variables_defaults',
	'$bind_variables'(Term, [], 0, _).


numbervars(Term, From, Next) :-
	set_bip_name(numbervars, 3),
	'$set_bind_variables_defaults',
	'$bind_variables'(Term, [], From, Next).
