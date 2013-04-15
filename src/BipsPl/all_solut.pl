/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : all_solut.pl                                                    *
 * Descr.: all solution collector management                               *
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

'$use_all_solut'.


findall(Template, Generator, Instances) :-
	'$findall'(Template, Generator, Instances, findall).

'$findall'(Template, Generator, Instances, Func) :-
	'$check_list_arg'(Instances, Func),
	'$store_solutions'(Template, Generator, Stop, Func),
	'$call_c_test'('Pl_Recover_Solutions_2'(Stop, 0, Instances)).




setof(Template, Goal, Instances) :-
	'$check_list_arg'(Instances, setof),
	'$bagof'(Template, Goal, Instances, setof).
%	sort(Instances).




bagof(Template, Generator, Instances) :-
	'$check_list_arg'(Instances, bagof),
	'$bagof'(Template, Generator, Instances, bagof).


'$bagof'(Template, Generator, Instances, Func) :-
	'$call_c_test'('Pl_Free_Variables_4'(Template, Generator, Generator1, Key)), !,
	'$store_solutions'(Key - Template, Generator1, Stop, Func),
	set_bip_name(Func, 3),   % for error too_many_variables in C function
	'$call_c_test'('Pl_Recover_Solutions_2'(Stop, 1, AllInstances)),
	(   Func = bagof ->
	    keysort(AllInstances)
	;   sort(AllInstances)
	),
	'$group_solutions'(AllInstances, Key, Instances).

'$bagof'(Template, _, Instances, Func) :-
	'$call_c'('Pl_Recover_Generator_1'(Generator)),
	'$findall'(Template, Generator, Instances, Func),
	Instances \== [],
	(   Func = bagof ->
	    true
	;   sort(Instances)
	).




'$store_solutions'(Template, Generator, Stop, Func) :-
	'$call_c'('Pl_Stop_Mark_1'(Stop)),
	(   '$call'(Generator, Func, 3, true),
	    '$call_c'('Pl_Store_Solution_1'(Template)),
	    fail
	;   true
	).




'$group_solutions'(AllInstances1, Key, Instances) :-
	'$call_c_test'('Pl_Group_Solutions_3'(AllInstances1, Key, Instances)).

'$group_solutions_alt' :-           % used by C code to create a choice-point
	'$call_c_test'('Pl_Group_Solutions_Alt_0').




         % Args testing

'$check_list_arg'(List, Func) :-
	set_bip_name(Func, 3),
	'$check_list_or_partial_list'(List).
