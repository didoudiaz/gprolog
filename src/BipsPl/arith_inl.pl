/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : arith_inl.pl                                                    * 
 * Descr.: arithmetic (inline) management - defs for meta-call             * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2010 Daniel Diaz                                     * 
 *                                                                         * 
 * GNU Prolog is free software; you can redistribute it and/or modify it   * 
 * under the terms of the GNU Lesser General Public License as published   * 
 * by the Free Software Foundation; either version 3, or any later version.* 
 *                                                                         * 
 * GNU Prolog is distributed in the hope that it will be useful, but       * 
 * WITHOUT ANY WARRANTY; without even the implied warranty of              * 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        * 
 * General Public License for more details.                                * 
 *                                                                         * 
 * You should have received a copy of the GNU Lesser General Public License* 
 * with this program; if not, write to the Free Software Foundation, Inc.  * 
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               * 
 *-------------------------------------------------------------------------*/

/* $Id$ */

:-	built_in.

'$use_arith_inl'.


X is Y :-
	X is Y.

X =:= Y :-
	X =:= Y.

X =\= Y :-
	X =\= Y.

X < Y :-
	X < Y.

X =< Y :-
	X =< Y.

X > Y :-
	X > Y.

X >= Y :-
	X >= Y.


'$arith_eval'(X, Y) :-
	'$call_c'('Pl_Arith_Eval_2'(X, Y)).
