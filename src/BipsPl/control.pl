/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : control.pl                                                      *
 * Descr.: control management                                              *
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

'$use_control'.




repeat.

repeat :-
	repeat.




abort :-
	'$call_c_jump'('Pl_Halt_If_No_Top_Level_1'(1)).


stop :-
	'$call_c_jump'('Pl_Halt_If_No_Top_Level_1'(0)).





halt :-
	halt(0).




halt(X) :-
	set_bip_name(halt, 1),
	'$call_c'('Pl_Halt_1'(X)).





between(L, U, X) :-
	set_bip_name(between, 3),
	'$call_c_test'('Pl_Between_3'(L, U, X)).

'$between_alt' :-
	'$call_c'('Pl_Between_Alt_0').



for(X, L, U) :-			% deprecated: use between/3 instead
	set_bip_name(for, 3),
	'$call_c_test'('Pl_Between_3'(L, U, X)).
