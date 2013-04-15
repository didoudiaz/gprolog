/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : expand.pl                                                       *
 * Descr.: term expansion management                                       *
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

'$use_expand'.



% all these must be steadfast (must work correctly if its output
% variable is already instantiated to the output value)

expand_term(T1, T3) :-
	'$expand_term1'(T1, T2),
	T2 = T3.

'$expand_term1'(T1, T2) :-
	(   var(T1),
	    T2 = T1
	;   '$call_term_expansion'(T1, T2)
	;   set_bip_name(expand_term, 2),
	    '$dcg_trans_rule'(T1, T2)
	;   T2 = T1
	), !.




'$call_term_expansion'(T1, T2) :-
	current_predicate(term_expansion / 2),
	call(term_expansion(T1, T2)).




phrase(DcgBody, In) :-
	'$phrase'(DcgBody, In, [], 2).




phrase(DcgBody, In, Out) :-
	'$phrase'(DcgBody, In, Out, 3).




'$phrase'(DcgBody, In, Out, Arity) :-
	set_bip_name(phrase, Arity),
        (   var(DcgBody) ->
	    '$pl_err_instantiation'
	;
	    true
	),
	% '$check_list_or_partial_list'(In),
	% '$check_list_or_partial_list'(Out),
	'$dcg_trans_body'(DcgBody, In, Out1, Body),
	'$call'(Body, phrase, Arity, true),
	Out = Out1.




'$dcg_trans_rule'(Dcg, Clause) :-
	'$call_c_test'('Pl_Dcg_Trans_Rule_2'(Dcg, Clause)).




'$dcg_trans_body'(DcgBody, In, Out, Body) :-
	'$call_c_test'('Pl_Dcg_Trans_Body_4'(DcgBody, In, Out, Body)).
