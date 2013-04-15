/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_infos.pl                                                     *
 * Descr.: FD variable information management                              *
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


:-	built_in_fd.

'$use_fd_infos'.


fd_vector_max(Max) :-
	set_bip_name(fd_vector_max, 1),
	'$call_c_test'('Pl_Fd_Vector_Max_1'(Max)).


fd_set_vector_max(Max) :-
	set_bip_name(fd_set_vector_max, 1),
	'$call_c'('Pl_Fd_Set_Vector_Max_1'(Max)).




fd_max_integer(Inf) :-
	set_bip_name(fd_max_integer, 1),
	'$call_c_test'('Pl_Fd_Max_Integer_1'(Inf)).




fd_min(Fdv, Min) :-
	set_bip_name(fd_min, 2),
	'$call_c_test'('Pl_Fd_Min_2'(Fdv, Min)).




fd_max(Fdv, Max) :-
	set_bip_name(fd_max, 2),
	'$call_c_test'('Pl_Fd_Max_2'(Fdv, Max)).




fd_dom(Fdv, List) :-
	set_bip_name(fd_domain, 2),
	'$call_c_test'('Pl_Fd_Dom_2'(Fdv, List)).




fd_size(Fdv, Size) :-
	set_bip_name(fd_size, 2),
	'$call_c_test'('Pl_Fd_Size_2'(Fdv, Size)).




fd_has_extra_cstr(Fdv) :-
	set_bip_name(fd_has_extra_cstr, 1),
	'$call_c_test'('Pl_Fd_Has_Extra_Cstr_1'(Fdv)).




fd_has_vector(Fdv) :-
	set_bip_name(fd_has_vector, 1),
	'$call_c_test'('Pl_Fd_Has_Vector_1'(Fdv)).




fd_use_vector(Fdv) :-
	set_bip_name(fd_use_vector, 1),
	'$call_c_test'('Pl_Fd_Use_Vector_1'(Fdv)).
