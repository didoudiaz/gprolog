/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : sort.pl                                                         *
 * Descr.: sort management                                                 *
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

'$use_sort'.


sort(List1, List2) :-
	set_bip_name(sort, 2),
	'$sys_var_write'(0, 0),                                    % 0 = sort
	'$call_c_test'('Pl_Sort_List_2'(List1, List2)).




msort(List1, List2) :-
	set_bip_name(msort, 2),
	'$sys_var_write'(0, 1),                                   % 1 = msort
	'$call_c_test'('Pl_Sort_List_2'(List1, List2)).




keysort(List1, List2) :-
	set_bip_name(keysort, 2),
	'$sys_var_write'(0, 2),                                 % 2 = keysort
	'$call_c_test'('Pl_Sort_List_2'(List1, List2)).


        % sort in-place predicates

sort(List) :-
	set_bip_name(sort, 1),
	'$sys_var_write'(0, 0),                                    % 0 = sort
	'$call_c'('Pl_Sort_List_1'(List)).




msort(List) :-
	set_bip_name(msort, 1),
	'$sys_var_write'(0, 1),                                   % 1 = msort
	'$call_c'('Pl_Sort_List_1'(List)).




keysort(List) :-
	set_bip_name(keysort, 1),
	'$sys_var_write'(0, 2),                                 % 2 = keysort
	'$call_c'('Pl_Sort_List_1'(List)).
