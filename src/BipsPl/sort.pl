/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : sort.pl                                                         * 
 * Descr.: sort management                                                 * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2005 Daniel Diaz                                     * 
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

'$use_sort'.


sort(List1, List2) :-
	set_bip_name(sort, 2),
	'$sys_var_write'(0, 0),                                    % 0 = sort
	'$call_c_test'('Sort_List_2'(List1, List2)).




sort0(List1, List2) :-
	set_bip_name(sort0, 2),
	'$sys_var_write'(0, 1),                                   % 1 = sort0
	'$call_c_test'('Sort_List_2'(List1, List2)).




keysort(List1, List2) :-
	set_bip_name(keysort, 2),
	'$sys_var_write'(0, 2),                                 % 2 = keysort
	'$call_c_test'('Sort_List_2'(List1, List2)).


        % sort in-place predicates

sort(List) :-
	set_bip_name(sort, 1),
	'$sys_var_write'(0, 0),                                    % 0 = sort
	'$call_c'('Sort_List_1'(List)).




sort0(List) :-
	set_bip_name(sort0, 1),
	'$sys_var_write'(0, 1),                                   % 1 = sort0
	'$call_c'('Sort_List_1'(List)).




keysort(List) :-
	set_bip_name(keysort, 1),
	'$sys_var_write'(0, 2),                                 % 2 = keysort
	'$call_c'('Sort_List_1'(List)).
