/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog to WAM compiler                                          */
/* File  : inst_codif.pl                                                   */
/* Descr.: instruction codification (needed for register allocation)       */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/


          /* alias stopping instructions */

alias_stop_instruction(InstW) :-
	functor(InstW, F, _),
	(   F=call
	;   F=execute
	), !.




          /* instruction codification */

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

codif(call(_/N), LCode) :-
	lst_r_for_call_execute(0, N, LCode).

codif(execute(_/N), LCode) :-
	lst_r_for_call_execute(0, N, LCode).

codif(load_cut_level(Tmp), [w(Tmp)]).

codif(cut(x(Tmp)), [r(Tmp)]).

codif(function(_, Reg, LReg), LCode) :-
	lst_r_for_function(LReg, Reg, LCode).

codif(call_c(_, LReg), LCode) :-
	lst_rw_for_call_c(LReg, [], LCode).

codif(call_c_test(_, LReg), LCode) :-
	lst_rw_for_call_c(LReg, [], LCode).

codif(call_c_jump(_, LReg), LCode) :-
	lst_rw_for_call_c(LReg, [], LCode).

codif(foreign_call_c(_, _, LReg, _), LCode) :-
	lst_rw_for_call_c(LReg, [], LCode).

	% instructions which use no temporaries

codif(_, []).




lst_r_for_call_execute(N, N, []).

lst_r_for_call_execute(I, N, [r(I)|L]) :-
	I1 is I+1,
	lst_r_for_call_execute(I1, N, L).




lst_r_for_function([], WReg, [w(WReg)]).

lst_r_for_function([Reg|LReg], WReg, [r(Reg)|LCode]) :-
	lst_r_for_function(LReg, WReg, LCode).




lst_rw_for_call_c([], End, End).

lst_rw_for_call_c([Reg|LReg], End, [r(Reg)|LCode]) :-
	lst_rw_for_call_c(LReg, [w(Reg)|End], LCode).


