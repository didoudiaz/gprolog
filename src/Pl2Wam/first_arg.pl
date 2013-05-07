/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : first_arg.pl                                                    *
 * Descr.: first argument detection                                        *
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


find_first_arg([], var).

find_first_arg([WamInst|WamCode], FirstArg) :-
	(   defines_first_arg(WamInst, FirstArg)
	;   stopping_inst(WamInst),
	    FirstArg = var
	;   find_first_arg(WamCode, FirstArg)
	), !.




stopping_inst(call(_)).

stopping_inst(execute(_)).

stopping_inst(cut(_)).

stopping_inst(soft_cut(_)).

stopping_inst(WamInst) :-
	codification(WamInst, LCode),
	assign_x0(LCode).




assign_x0([Code|LCode]) :-
	(   Code = w(0)
	;   Code = c(R1, R2),
	    R1 \== R2,
	    R2 = 0
	;   assign_x0(LCode)
	).




defines_first_arg(get_atom(A, 0), atm(A)).

defines_first_arg(get_integer(N, 0), int(N)).

%defines_first_arg(get_float(N,0),flt(N)).            % no indexing on floats

defines_first_arg(get_nil(0), atm([])).

defines_first_arg(get_list(0), lst).

defines_first_arg(get_structure(F / N, 0), stc(F, N)).
