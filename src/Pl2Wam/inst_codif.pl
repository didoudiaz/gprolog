/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : inst_codif.pl                                                   *
 * Descr.: instruction codification (needed for register allocation)       *
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


          % alias stopping instructions

alias_stop_instruction(InstW) :-
	functor(InstW, F, _),
	(   F = call
	;   F = execute
        ;   F = call_c,
	    arg(2, InstW, LCOpt),
	    (memberchk(jump, LCOpt) ; memberchk(use_x_args, LCOpt))
	), !.




          % instruction codification

codification(WamInst, LCode) :-
	codif(WamInst, LCode), !.


codif(get_variable(x(Tmp), Arg), [c(Arg, Tmp)]).

codif(get_value(x(Tmp), Arg), [r(Tmp), r(Arg)]).

codif(get_variable(y(_), Arg), [r(Arg)]).

codif(get_value(y(_), Arg), [r(Arg)]).

codif(get_atom(_, Arg), [r(Arg)]).

codif(get_integer(_, Arg), [r(Arg)]).

codif(get_float(_, Arg), [r(Arg)]).

codif(get_nil(Arg), [r(Arg)]).

codif(get_list(Reg), [r(Reg)]).

codif(get_structure(_, Reg), [r(Reg)]).

codif(put_variable(x(Tmp), Arg), [w(Tmp), w(Arg)]).

codif(put_void(Arg), [w(Arg)]).

codif(put_value(x(Tmp), Arg), [c(Tmp, Arg)]).

codif(put_variable(y(_), Arg), [w(Arg)]).

codif(put_value(y(_), Arg), [w(Arg)]).

codif(put_unsafe_value(y(_), Arg), [w(Arg)]).

codif(put_atom(_, Arg), [w(Arg)]).

codif(put_integer(_, Arg), [w(Arg)]).

codif(put_float(_, Arg), [w(Arg)]).

codif(put_nil(Arg), [w(Arg)]).

codif(put_list(Reg), [w(Reg)]).

codif(put_structure(_, Reg), [w(Reg)]).

codif(math_load_value(x(Reg), Tmp), [r(Reg), w(Tmp)]).

codif(math_load_value(y(_), Tmp), [w(Tmp)]).

codif(math_fast_load_value(x(Reg), Tmp), [r(Reg), w(Tmp)]).

codif(math_fast_load_value(y(_), Tmp), [w(Tmp)]).

codif(unify_variable(x(Tmp)), [w(Tmp)]).

codif(unify_value(x(Tmp)), [r(Tmp)]).

codif(unify_local_value(x(Tmp)), [r(Tmp)]).

codif(call(T), LCode) :-
	( T = _ / N ; T = _:_/N ), !,
	lst_r_for_call_execute(0, N, LCode).

codif(execute(T), LCode) :-
	( T = _ / N ; T = _:_/N ), !,
	lst_r_for_call_execute(0, N, LCode).

codif(get_current_choice(x(Tmp)), [w(Tmp)]).

codif(cut(x(Tmp)), [r(Tmp)]).

codif(soft_cut(x(Tmp)), [r(Tmp)]).

codif(call_c(_, LCOpt, LReg), LCode) :-
	(   member(x(Tmp), LCOpt) ->
	    End = [w(Tmp)]
	;   End = []
        ),
	lst_rw_for_c_call(LReg, End, LCode).

codif(foreign_call_c(_, _, LReg, _), LCode) :-
	lst_rw_for_foreign_c_call(LReg, [], LCode).

	% instructions which use no temporaries

codif(_, []).




lst_r_for_call_execute(N, N, []).

lst_r_for_call_execute(I, N, [r(I)|L]) :-
	I1 is I + 1,
	lst_r_for_call_execute(I1, N, L).




lst_rw_for_foreign_c_call([], End, End).

lst_rw_for_foreign_c_call([Reg|LReg], End, [r(Reg)|LCode]) :-
	lst_rw_for_foreign_c_call(LReg, [w(Reg)|End], LCode).




lst_rw_for_c_call([], End, End).

lst_rw_for_c_call([x(Reg)|LReg], End, [r(Reg)|LCode]) :-
	!,
	lst_rw_for_c_call(LReg, [w(Reg)|End], LCode).

lst_rw_for_c_call([_|LReg], End, LCode) :-
	lst_rw_for_c_call(LReg, End, LCode).

