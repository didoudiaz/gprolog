/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog buit-in predicates                                       */
/* File  : oper.pl                                                         */
/* Descr.: operator management                                             */
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

:-	built_in.

'$use_oper'.

op(Prec, Specif, Oper) :-
	set_bip_name(op, 3),
	'$check_atom_or_atom_list'(Oper),
	(   atom(Oper) ->
	    '$op2'(Prec, Specif, Oper)
	;   '$op1'(Oper, Specif, Prec)
	).


'$op1'([], _, _).

'$op1'([Oper|LOper], Specif, Prec) :-
	'$op2'(Prec, Specif, Oper),
	'$op1'(LOper, Specif, Prec).


'$op2'(Prec, Specif, Oper) :-
	'$call_c'('Op_3'(Prec, Specif, Oper)).




current_op(Prec, Specif, Oper) :-
	set_bip_name(current_op, 3),
	'$call_c_test'('Current_Op_3'(Prec, Specif, Oper)).


'$current_op_alt' :-                             % used by C code to create a choice-point
	'$call_c_test'('Current_Op_Alt_0').
