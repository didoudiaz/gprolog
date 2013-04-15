/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : print.pl                                                        *
 * Descr.: term print management                                           *
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

'$use_print'.


print(Term) :-
	set_bip_name(print, 1),
	'$call_c'('Pl_Print_1'(Term)).

print(SorA, Term) :-
	set_bip_name(print, 2),
	'$call_c'('Pl_Print_2'(SorA, Term)).




'$try_portray'(Term) :-
	'$current_predicate'(portray / 1),
	g_assign('$portray_ok', 0),
	'$catch'((portray(Term), !), Err, (format(top_level_output, 'exception from portray/1: ~q~n', [Err]), fail), portray, 1, false),
	g_assign('$portray_ok', 1),
	fail.

'$try_portray'(_) :-
	g_read('$portray_ok', 1).




get_print_stream(Stream) :-
	set_bip_name(get_print_stream, 1),
	'$check_stream_or_var'(Stream, Stm),
	'$call_c'('Pl_Get_Print_Stm_1'(Stm)).
