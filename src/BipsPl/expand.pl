/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : expand.pl                                                       * 
 * Descr.: term expansion management                                       * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2007 Daniel Diaz                                     * 
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

'$use_expand'.


expand_term(T1, T3) :-                                    % must be steadfast
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
	'$check_list_or_partial_list'(In),
	'$check_list_or_partial_list'(Out),
	'$dcg_trans_body'(DcgBody, In, Out, Body),
	'$call'(Body, phrase, Arity, true).




'$dcg_trans_rule'(Dcg, Clause) :-
	'$call_c_test'('Dcg_Trans_Rule_2'(Dcg, Clause)).




'$dcg_trans_body'(DcgBody, In, Out, Body) :-
	'$call_c_test'('Dcg_Trans_Body_4'(DcgBody, In, Out, Body)).
