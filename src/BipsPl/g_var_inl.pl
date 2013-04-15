/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : g_var_inl.pl                                                    *
 * Descr.: global variable (inline) management - defs for meta-call        *
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


