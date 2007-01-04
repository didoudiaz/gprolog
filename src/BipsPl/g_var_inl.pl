/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : g_var_inl.pl                                                    * 
 * Descr.: global variable (inline) management - defs for meta-call        * 
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

'$use_g_var_inl'.


g_assign(Var, Value) :-
	g_assign(Var, Value).




g_assignb(Var, Value) :-
	g_assignb(Var, Value).




g_link(Var, Value) :-
	g_link(Var, Value).




g_read(Var, Value) :-
	g_read(Var, Value).




g_array_size(Var, Type) :-
	g_array_size(Var, Type).



g_inc(X) :-
	g_inc(X).


g_inco(X, Y) :-
	g_inco(X, Y).


g_inc(X, Y) :-
	g_inc(X, Y).


g_inc(X, Y, Z) :-
	g_inc(X, Y, Z).




g_dec(X) :-
	g_dec(X).


g_deco(X, Y) :-
	g_deco(X, Y).


g_dec(X, Y) :-
	g_dec(X, Y).


g_dec(X, Y, Z) :-
	g_dec(X, Y, Z).




g_set_bit(X, Y) :-
	g_set_bit(X, Y).




g_reset_bit(X, Y) :-
	g_reset_bit(X, Y).




g_test_set_bit(X, Y) :-
	g_test_set_bit(X, Y).




g_test_reset_bit(X, Y) :-
	g_test_reset_bit(X, Y).


