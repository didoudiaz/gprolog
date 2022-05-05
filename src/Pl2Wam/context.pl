/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : context.pl                                                      *
 * Descr.: contextual LP management                                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2022 Daniel Diaz                                     *
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

/*-------------------------------------------------------------------------*
 * This file implements a support for contextual LP. It is a prototype and *
 * should be considered as an alpha version. One main requierement was to  *
 * modify the existing compiler code as little as possible and to group    *
 * all context-specific code in order to obtain a little diff vs derived   *
 * gprolog version. This imposed some limitation to context support in the *
 * compiler. Mainly, the compiler expects at most 1 unit/1 declaration per *
 * source at the beginning of file. However several units are allowed, but *
 * the following restrictions apply then (among others not yet known...):  *
 *                                                                         *
 *   2 predicates with same func/arity in different units are not accepted *
 *   discontiguous predicates are not well compiled                        *
 *   a dynamic predicate is associated to the last unit encountered        *
 *                                                                         *
 * To compile with context support, the user should set an environment     *
 * variable: GPROLOG_CXT which can contain the following sub-strings:      *
 *                                                                         *
 *    off: turn off context support                                        *
 *                                                                         *
 *    minimal: context calls for predicates outside the current unit.      *
 *    recursive: context calls for any predicate except if bip or recursive*
 *    medium: context call for any predicate except bips.                  *
 *    full: context call for all predicate (including bips).               *
 *                                                                         *
 *    debug: display how clauses are rewritten for context support         *
 *                                                                         *
 * Default: recursive                                                      *
 *                                                                         *
 * NB: if you use full you should ensure the needed built-in are linked    *
 * (e.g. do not use --min-pl-bips / --min-fd-bips / --min-bips,...)        *
 * If you want to recompile pl2wam itself with 'full', replace in the      *
 * Makefile --min-bips by --no-top-level.                                  *
 * ------------------------------------------------------------------------*/

:- op(600, xfx, [:<]).		% useless if boot-strapped but useful
:- op(600, xfy, [:>]).		% when compiled with regular GNU-Prolog
:- op(600, xfy, [::]).		% ...
:- op(600, fy, [:<, :^, :#, :>]). % ...
				

'$cxt_for_call'(_).		% FIXME: should never be needed!!!
 
% --- First pass - keep info about defined predicates --- %

cxt_first_pass(_) :-
	cxt_parse_env_variable,
	g_read(cxt_support, f),
	!.

cxt_first_pass(PlFile) :-
	g_assign(cxt_unit, []),
	retractall(cxt_def(_, _, _)),
	sr_open(PlFile, D, [include(hide), op(hide), restart(true),
			    write_error(false), undo_directives(true)]),
	repeat,
	sr_read_term(D, T, [], SRError),
	SRError = sr_ok,
	nonvar(T),
	(   T = end_of_file -> !
	;
	    expand_term(T, T1),
	    cxt_treat_term(T1),
	    fail
	),
	g_assign(cxt_unit, []),
	g_assign(cxt_unit_args, []),
	sr_close(D).


cxt_treat_term((:- D)) :-
	(   D = unit(Unit),
	    callable(D) ->
	    functor(Unit, U, Arity),
	    g_assign(cxt_unit, U),
	    g_assign(cxt_unit_arity, Arity)
	;
	    true
	), !.

cxt_treat_term((H :- _)) :-
	!,
	functor(H, F, N),
	cxt_add_def(F, N).

cxt_treat_term(H) :-
	functor(H, F, N),
	cxt_add_def(F, N).



cxt_add_def(F, N) :-
	g_read(cxt_unit, UP),
	g_read(cxt_unit_arity, A),
	cxt_add_def(F, N, UP/A).

cxt_add_def(F, N, U) :-
	cxt_test_def(F, N, U),
	!.

cxt_add_def(F, N, U) :-
	( g_read(cxt_env_debug, 1) -> format('% adding ~w\n', [cxt_def(F,N,U)]) ; true ),
	assertz(cxt_def(F, N, U)).

cxt_test_def(F, N, U) :-
	clause(cxt_def(F, N, U), _),
	!.


% --- environment variable parsing --- %

cxt_parse_env_variable :-
	g_assign(cxt_support, f),
	g_assign(cxt_env_call, 0),
	g_assign(cxt_env_debug, f),
	( environ('GPROLOG_CXT', V) -> true ; V='' ),
	\+ sub_atom(V, _, _, _, off),
	g_assign(cxt_support, t),
	(   sub_atom(V, _, _, _, recursive),
	    g_assign(cxt_env_call, 1),
	    Call = recursive
	;   sub_atom(V, _, _, _, medium),
	    g_assign(cxt_env_call, 2),
	    Call = medium
	;   sub_atom(V, _, _, _, full),
	    g_assign(cxt_env_call, 3),
	    Call = full
	;   sub_atom(V, _, _, _, minimal),
	    Call = minimal
	;   g_assign(cxt_env_call, 1), % defaults to "recursive",
	    Call = recursive	% which is quite safe.
	),
	(   sub_atom(V, _, _, _, debug),
	    g_assign(cxt_env_debug, t),
	    Debug = on
	;   Debug = off
	), !,
	(   Debug = on ->
	    format('Context support with call: ~w  debug: ~w~n',
		   [Call, Debug])
	;   true
	).

cxt_parse_env_variable.



% --- term rewriting --- %

cxt_rewrite_term(Cl, SingNames, _, Cl, SingNames) :-
	g_read(cxt_support, f), !.

cxt_rewrite_term(Cl, SingNames, _, Cl, SingNames) :-
	var(Cl), !.

cxt_rewrite_term(Cl, SingNames, _, Cl, SingNames) :-
	Cl = end_of_file,
	!.

cxt_rewrite_term((:- D), SingNames, VN, Cl, SingNames1) :-
	cxt_handle_directive(D, SingNames, VN, Cl, SingNames1), !.

cxt_rewrite_term(Cl, SingNames, VN, Cl1, SingNames1) :-
	cxt_rewrite_clause(Cl, SingNames, VN, Cl1, SingNames1).




% --- directive handling --- %

cxt_handle_directive(unit(Unit), _, VN, '$$cxt_unit$$'(U/Arity), []) :-
	callable(Unit),
	Unit =.. [U|LVar],
	functor(Unit, U, Arity),
	cxt_mk_unit_args(LVar, VN, LUnitArg), !,
	g_assign(cxt_unit, U),
	g_assign(cxt_unit_arity, Arity),
	g_assign(cxt_unit_args, LUnitArg).

cxt_handle_directive(D, _, _, _, _) :-
	D = unit(_), !,
	error('invalid directive ~q', [D]).

cxt_handle_directive(initialization(I), SingNames, VN, (:- initialization(I1)), SingNames1) :-
	!,
	cxt_rewrite_clause(('$cxt' :- I), SingNames, VN, ('$cxt' :- I1), SingNames1).

cxt_handle_directive(D, SingNames, _, (:- D), SingNames).




cxt_mk_unit_args([], _, []).

cxt_mk_unit_args([Var|LVar], VN, [Name=Pos|LUnitArg]) :-
	cxt_find_var(VN, Name, Var, 1, Pos),
	cxt_mk_unit_args(LVar, VN, LUnitArg).




% cxt_find_var(L, N, V, P1, P2) :-
% 	SELF=cxt_find_var(L, N, V, P1, P2),
% 	format("[in: ~w]\n", [SELF]),
% 	cxt_find_var1(L, N, V, P1, P2),
% 	format("[out: ~w]\n", [SELF]).
% cxt_find_var(L, N, V, P1, P2) :-
% 	SELF=cxt_find_var(L, N, V, P1, P2),
% 	format("[fail: ~w]\n", [SELF]),
% 	fail.

cxt_find_var(VARARGLIST, _, _, _, _) :-
	var(VARARGLIST), !,
	format("[internal error: variable unit argument list!\n", []),
	fail.

cxt_find_var([Name=Var|_], Name, Var1, Pos, Pos) :-
	Var == Var1, !.

cxt_find_var([Name=Var|_], Name1, Var, Pos, Pos) :- % for reverse usage
	Name == Name1, !.

cxt_find_var([_|VN], Name, Var, Pos, Pos2) :-
	Pos1 is Pos + 1,
	cxt_find_var(VN, Name, Var, Pos1, Pos2).




% --- clause rewriting --- %

cxt_rewrite_clause(Cl, SingNames, VN, (Head :- Body3), SingNames1) :-
	g_link(cxt_var_names, VN),
	g_read(cxt_unit_args, LUnitArgs),
	g_link(cxt_unit_args_ln, LUnitArgs),
	cxt_unit_args_in_term(Cl, SingNames, LUsed, SingNames1),
	(   Cl = (Head :- Body)
	;   Cl = Head, Body = true), !,
	nonvar(Head),
	functor(Head, F, N),
	g_assign(cxt_cur_func, F),
	g_assign(cxt_cur_arity, N),
	(   cxt_test_def(F, N, U) -> % '$cxt' :- ... from initialization/1
	    g_assign(cxt_owner_unit, U)
	;   g_assign(cxt_owner_unit, _)	% any to be unified
	),
	g_link(cxt_var, CxtVar),
	g_link(cxt_calling_var, CxtCallingVar),
	g_assign(cxt_force_cxt_call, f),
	cxt_rewrite_body(Body, Body1),
	(   LUsed = [] ->	% close end var
	    Body2 = Body1
	;   cxt_body_of_unit_args(LUsed, CxtVarCar, BodyUnitArgs),
	    cxt_mk_arg3_call(1, CxtVar, CxtVarCar, P),
	    Body2 = (P, BodyUnitArgs, Body1)
	),
	Body3 = ('$cxt_get_K'(CxtVar), '$cxt_get_CK'(CxtCallingVar), Body2),
	cxt_debug_displ_clause((Head :- Body3)).

cxt_rewrite_clause(Head, SingNames, VN, Cl, SingNames1) :-
	cxt_rewrite_clause((Head :- true), SingNames, VN, Cl, SingNames1).




cxt_unit_args_in_term(T, SingNames, LUsed, SingNames1) :-
	var(T),
	T = Var,
	g_read(cxt_var_names, VN),
	cxt_find_var(VN, Name, Var, 1, _),
	g_read(cxt_unit_args_ln, LUnitArgs),
	cxt_find_var(LUnitArgs, Name, Pos, 1, _), !,
	member(Var=Pos, LUsed), !, % find or add with end var
	delete(SingNames, Name=Var, SingNames1). %succeeds if does not belong

cxt_unit_args_in_term(T, SingNames, LUsed, SingNames1) :-
	compound(T), !,
	functor(T, _, N),
	cxt_unit_args_in_subterm(0, N, T, SingNames, LUsed, SingNames1).

cxt_unit_args_in_term(_, SingNames, _, SingNames).




cxt_unit_args_in_subterm(N, N, _, SingNames, _, SingNames) :- !.

cxt_unit_args_in_subterm(I, N, T, SingNames, LUsed, SingNames2) :-
	I1 is I + 1,
	arg(I1, T, Arg),
	cxt_unit_args_in_term(Arg, SingNames, LUsed, SingNames1),
	cxt_unit_args_in_subterm(I1, N, T, SingNames1, LUsed, SingNames2).


	

cxt_body_of_unit_args([], _, true):- !. % close end variable

cxt_body_of_unit_args([Var=Pos|LUsed], CxtVarCar, (P, Body)) :-
	cxt_mk_arg3_call(Pos, CxtVarCar, Var, P),
	cxt_body_of_unit_args(LUsed, CxtVarCar, Body).



cxt_mk_arg3_call(ArgNo, Term, SubTerm, P) :-
%	P = arg(ArgNo, Term, SubTerm).
%	P = '$call_c'('Cxt_Arg_3'(ArgNo, Term, SubTerm), [fast_call, boolean, by_value]).

	P = '$cxt_arg'(ArgNo, Term, SubTerm).


% -----------------------------------------------------------------------------

cxt_rewrite_body(Body, Body1) :-
	var(Body), !,
	cxt_rewrite_simple(Body, Body1).

cxt_rewrite_body((P, Q), (P1, Q1)) :-
	cxt_rewrite_body(P, P1),
	cxt_rewrite_body(Q, Q1).

cxt_rewrite_body((P ; Q), R) :-
	cxt_rewrite_body(P, P1),
	cxt_rewrite_body(Q, Q1),
	T = (P1 ; Q1),
	g_read(cxt_owner_unit, U), % because ; gives rise to an aux predicate
	(   U = []/0 ->
%%	    g_read(cxt_var, CxtVar),
%%	    R = ('$cxt_put_K'(CxtVar), T)
	    R = T
	;
	    R = ('$cxt_unit_for_next_call'(U), T)
	).

cxt_rewrite_body((P -> Q), (P1 -> Q1)) :-
	cxt_rewrite_body(P, P1),
	cxt_rewrite_body(Q, Q1).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/* what to do with control constructs ???
cxt_rewrite_body(call(P), _).

cxt_rewrite_body(catch(G, C, R), _).

cxt_rewrite_body(throw(B), _).
*/

cxt_rewrite_body(call(G), G1) :-
	cxt_rewrite_body(G, G1).

% cxt_rewrite_body(catch(G, C, R), catch(G1, C, R1)) :-
% 	G1=foo(G),
% 	R1=R.
%	g_read(cxt_var, CxtVar),
%	cxt_rewrite_body(CxtVar :< G, G1),
%	cxt_rewrite_body(CxtVar :< R, R1).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

cxt_rewrite_body(C :> P, (CxtVarNew = CxtNew, P2)) :-
	g_read(cxt_var, CxtVarOld),
	CxtNew = [C|CxtVarOld],
	g_link(cxt_var, CxtVarNew),
	g_read(cxt_force_cxt_call, CxtForce),
	g_assign(cxt_force_cxt_call, t),
	cxt_rewrite_body(P, P1),
	g_assign(cxt_force_cxt_call, CxtForce),
	g_link(cxt_var, CxtVarOld),
	P2 = P1.

cxt_rewrite_body(C :< P, (CxtVarNew = CxtNew, P2)) :-
	g_read(cxt_var, CxtVarOld),
	CxtNew = C,
	g_link(cxt_var, CxtVarNew),
	g_read(cxt_force_cxt_call, CxtForce),
	g_assign(cxt_force_cxt_call, t),
	cxt_rewrite_body(P, P1),
	g_assign(cxt_force_cxt_call, CxtForce),
	g_link(cxt_var, CxtVarOld),
	P2 = P1.

cxt_rewrite_body(U :: P, ('$cxt_locate'(CxtVarOld, U, C), P2)) :-
	g_read(cxt_var, CxtVarOld),
	g_link(cxt_var, C),
	g_read(cxt_force_cxt_call, CxtForce),
	g_assign(cxt_force_cxt_call, t),
	cxt_rewrite_body(C :< P, P1),
	g_assign(cxt_force_cxt_call, CxtForce),
	g_link(cxt_var, CxtVarOld),
	P2 = P1.

cxt_rewrite_body(:# P, P2) :-
	g_read(cxt_var, CxtVarOld),
	g_read(cxt_calling_var, C), % calling context (x254)
	g_link(cxt_var, C),
	g_read(cxt_force_cxt_call, CxtForce),
	g_assign(cxt_force_cxt_call, t),
	cxt_rewrite_body(P, P1),
	g_assign(cxt_force_cxt_call, CxtForce),
	g_link(cxt_var, CxtVarOld),
	P2 = P1.

cxt_rewrite_body(:^ P, (CxtVarOld = [_|C], P2)) :-
	g_read(cxt_var, CxtVarOld),
	g_link(cxt_var, C),
	g_read(cxt_force_cxt_call, CxtForce),
	g_assign(cxt_force_cxt_call, t),
	cxt_rewrite_body(P, P1),
	g_assign(cxt_force_cxt_call, CxtForce),
	g_link(cxt_var, CxtVarOld),
	P2 = P1.


cxt_rewrite_body(P, P1) :-
	cxt_rewrite_simple(P, P1).



% -----------------------------------------------------------------------------

cxt_rewrite_simple(P, C = CxtVar) :-
	nonvar(P),
	P = context(C), !,
	g_read(cxt_var, CxtVar).

cxt_rewrite_simple(P, C = CxtVar) :-
	nonvar(P),
	P = :< C, !,
	g_read(cxt_var, CxtVar).

cxt_rewrite_simple(P, C = CxtCallingVar) :-
	nonvar(P),
	P = ccontext(C), !,
	g_read(cxt_calling_var, CxtCallingVar).

cxt_rewrite_simple(P, C = CxtCallingVar) :-
	nonvar(P),
	P = :> C, !,
	g_read(cxt_calling_var, CxtCallingVar).


% cxt_rewrite_simple(P, g_read('$$hcontext$$', H)) :-
% 	nonvar(P),
% 	P = hcontext(H), !.

cxt_rewrite_simple(P, P1) :-
	nonvar(P),
	functor(P, F, N),
	cxt_useless_for_call(F, N), !,
	g_read(cxt_var, CxtVar),
	P1 = ('$cxt_put_K'(CxtVar), P).

cxt_rewrite_simple(P, P2) :-
	g_read(cxt_force_cxt_call, f),
	nonvar(P),
	functor(P, F, N),
	(   g_read(cxt_env_call, 0)
	;   g_read(cxt_env_call, 1), % recursive
	    g_read(cxt_cur_func, F),
	    g_read(cxt_cur_arity, N)
	), !,
	cxt_test_def(F, N, U),
	g_read(cxt_owner_unit, U1),
	(   U = []/0 ->
	    P1 = P
	;   U = U1,
	    P1 = ('$cxt_unit_for_next_call'(U), P)
	),
	g_read(cxt_var, CxtVar),
	P2 = ('$cxt_put_K'(CxtVar), P1).

cxt_rewrite_simple(P, ('$cxt_for_call'(CxtVar), P)) :-
	g_read(cxt_var, CxtVar).





cxt_useless_for_call(F, N) :-
        bip(F, N), !,		% built-in
	g_read(cxt_env_call, C),
	C < 3.

cxt_useless_for_call(F, N) :-
        control_construct(F, N), !. % control constructs

cxt_useless_for_call(F, _) :-
        sub_atom(F, 0, 1, _, $). % predicate starting with '$'




cxt_debug_displ_clause(Cl) :-
	g_read(cxt_env_debug, t),
	g_read(cxt_var_names, VN),
	name_query_vars(VN, _),
	portray_clause(Cl), nl,
	fail.			% to undo bindings

cxt_debug_displ_clause(_).




% --- code emission --- %

				% should be in wam_emit.pl
cxt_emit_unit_name(U) :-
	g_read(streamwamfile, Stream),
	format(Stream, '~n~nunit_name(~q).~n', [U]).
