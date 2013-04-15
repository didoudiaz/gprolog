/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_math.pl                                                      *
 * Descr.: mathematical predicate management                               *
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

'$use_fd_math'.


LE #= RE :-
	set_bip_name(#=, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(0)),
	'$call_c_test'('Pl_Fd_Eq_2'(LE, RE)).

LE #\= RE :-
	set_bip_name(#\=, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(0)),
	'$call_c_test'('Pl_Fd_Neq_2'(LE, RE)).

LE #< RE :-
	set_bip_name(#<, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(0)),
	'$call_c_test'('Pl_Fd_Lt_2'(LE, RE)).

LE #=< RE :-
	set_bip_name(#=<, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(0)),
	'$call_c_test'('Pl_Fd_Lte_2'(LE, RE)).

LE #> RE :-
	set_bip_name(#>, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(0)),
	'$call_c_test'('Pl_Fd_Lt_2'(RE, LE)).

LE #>= RE :-
	set_bip_name(#>=, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(0)),
	'$call_c_test'('Pl_Fd_Lte_2'(RE, LE)).




LE #=# RE :-
	set_bip_name(#=, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(1)),
	'$call_c_test'('Pl_Fd_Eq_2'(LE, RE)).

LE #\=# RE :-
	set_bip_name(#\=, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(1)),
	'$call_c_test'('Pl_Fd_Neq_2'(LE, RE)).

LE #<# RE :-
	set_bip_name(#<, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(1)),
	'$call_c_test'('Pl_Fd_Lt_2'(LE, RE)).

LE #=<# RE :-
	set_bip_name(#=<, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(1)),
	'$call_c_test'('Pl_Fd_Lte_2'(LE, RE)).

LE #># RE :-
	set_bip_name(#>, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(1)),
	'$call_c_test'('Pl_Fd_Lt_2'(RE, LE)).

LE #>=# RE :-
	set_bip_name(#>=, 2),
	'$call_c'('Pl_Fd_Set_Full_Ac_Flag_1'(1)),
	'$call_c_test'('Pl_Fd_Lte_2'(RE, LE)).
