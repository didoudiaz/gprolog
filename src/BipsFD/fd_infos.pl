/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : FD constraint solver buit-in predicates                         * 
 * File  : fd_infos.pl                                                     * 
 * Descr.: FD variable information management                              * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2004 Daniel Diaz                                     * 
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
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     * 
 *-------------------------------------------------------------------------*/

/* $Id$ */

:-	built_in_fd.

'$use_fd_infos'.


fd_vector_max(Max) :-
	set_bip_name(fd_vector_max, 1),
	'$call_c_test'('Fd_Vector_Max_1'(Max)).


fd_set_vector_max(Max) :-
	set_bip_name(fd_set_vector_max, 1),
	'$call_c'('Fd_Set_Vector_Max_1'(Max)).




fd_max_integer(Inf) :-
	set_bip_name(fd_max_integer, 1),
	'$call_c_test'('Fd_Max_Integer_1'(Inf)).




fd_min(Fdv, Min) :-
	set_bip_name(fd_min, 2),
	'$call_c_test'('Fd_Min_2'(Fdv, Min)).




fd_max(Fdv, Max) :-
	set_bip_name(fd_max, 2),
	'$call_c_test'('Fd_Max_2'(Fdv, Max)).




fd_dom(Fdv, List) :-
	set_bip_name(fd_domain, 2),
	'$call_c_test'('Fd_Dom_2'(Fdv, List)).




fd_size(Fdv, Size) :-
	set_bip_name(fd_size, 2),
	'$call_c_test'('Fd_Size_2'(Fdv, Size)).




fd_has_extra_cstr(Fdv) :-
	set_bip_name(fd_has_extra_cstr, 1),
	'$call_c_test'('Fd_Has_Extra_Cstr_1'(Fdv)).




fd_has_vector(Fdv) :-
	set_bip_name(fd_has_vector, 1),
	'$call_c_test'('Fd_Has_Vector_1'(Fdv)).




fd_use_vector(Fdv) :-
	set_bip_name(fd_use_vector, 1),
	'$call_c_test'('Fd_Use_Vector_1'(Fdv)).
