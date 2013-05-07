/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : reg_alloc.pl                                                    *
 * Descr.: pass 4: register allocation                                     *
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


/*-------------------------------------------------------------------------*
 * The main predicate is:                                                  *
 * allocate_registers(LInstW) ou allocate_registers(LInstW,MaxRegUsed):    *
 *    where LInstW is a list of instructions.                              *
 *    and MaxRegUsed is an integer corresponding to the greatest register  *
 *    used (-1 if none or n>=0 if reg0...regMaxRegUsed are used).          *
 *                                                                         *
 * Two predicates must be provided in addition to the allocater:           *
 *                                                                         *
 * codification(InstW, LCode):                                             *
 *    defines the action of InstW on the registers as a list LCode of codes*
 *    c(R1, R2) (copy R1 into R2), r(R) (read R) or w(R) (write R).        *
 *                                                                         *
 * alias_stop_instruction(InstW):                                          *
 *     true if InstW stop aliasing propagation.                            *
 *                                                                         *
 * Terminology:                                                            *
 *     Arg: Arg is an argument iff integer(Arg)                            *
 *     Tmp: Tmp is a temporary iff var(Tmp)                                *
 *     Reg: Reg is a register if it is either an argument or a temporary.  *
 *                                                                         *
 * This allocation proceeds in 3 steps:                                    *
 *                                                                         *
 *  1) computing aliases (i.e. list of same values at entry of each inst): *
 *     LAlias is a list of aliases (one for each instruction)              *
 *     LAlias = [Alias,...]                                                *
 *     The aliases (Alias) are represented as a set of same values (LSame) *
 *     Alias = [LSame,...].                                                *
 *     each LSame is a set of Regs (integers or variables)                 *
 *     eg Alias = [[1,2,X,Y],[3,Z,4]] means 1,2,X,Y are aliased, 3,Z,4 also*
 *                                                                         *
 *  2) computing the list of temporaries LTmp=[tmp(Tmp, Imposs, Wish),...] *
 *     where Imposs is a set of impossible values and Wish a set of wanted *
 *     values (to give rise to useless copy instructions).                 *
 *     The code is traversed in reverse order, computing at each time the  *
 *     set of Regs in life (InLife) (see PhD Thesis of Mats Carlsson).     *
 *                                                                         *
 *  3) Each Tmp in LTmp is assigned w.r.t. to Wish and Imposs in 2 steps:  *
 *                                                                         *
 *     a) from [tmp(Tmp, Imposs, Wish)|LTmp]:                              *
 *                                                                         *
 *        while there exists Tmpj in Wish and not in Imposs:               *
 *           let tmp(Tmpj, Impossj, Wishj) be the associated record in LTmp*
 *           Imposs := Imposs + Impossj and Wish := Wish + Wishj,          *
 *           LTmp := LTmp-tmp(Tmpj, Impossj, Wishj) (remove Tmpj from LTmp)*
 *           Tmpj = Tmp (unify them)                                       *
 *                                                                         *
 *        At the end of the loop:                                          *
 *        if there exists an integer k in Wish-Imposs then  (see NB below) *
 *           Tmp = k else replace tmp(Tmp, Imposs, Wish) in LTmp           *
 *                                                                         *
 *     b) for each Tmp remaining in LTmp assign a value w.r.t to Imposs    *
 *        by chosing the first integer not present in Imposs (after sort)  *
 *                                                                         *
 * NB: it seems, from the construction, that, in Wish, only remains        *
 * possible values so the compl(Wish, Imposs, AssignOK) would be useless,  *
 * but I have to check this in depth.                                      *
 *-------------------------------------------------------------------------*/

allocate_registers(LInstW) :-
	allocate_registers(LInstW, _).


allocate_registers(LInstW, MaxRegUsed) :-
	g_read(reg_opt, OptReg),
	(   OptReg > 0 ->
	    aliases(LInstW, [], LAlias)
	;   true
	),
	create_lst_tmp(LInstW, LAlias, _, LTmp),
	assign_lst_tmp(LTmp, MaxRegUsed).




          % Aliasing information creation

aliases([], _, []).

aliases([InstW|LInstW], Alias, [Alias|LAlias]) :-
	(   alias_stop_instruction(InstW) ->
	    Alias1 = []
	;   codification(InstW, LCode),
	    aliases1(LCode, Alias, Alias1)
	), !,
	aliases(LInstW, Alias1, LAlias).


aliases1([], Alias, Alias).

aliases1([Code|LCode], Alias, Alias3) :-
	(   Code = r(Reg),
	    Alias2 = Alias
	;   Code = w(Reg),
	    remove_aliases_of(Alias, Reg, Alias2)
	;   Code = c(Reg, Reg1),
	    remove_aliases_of(Alias, Reg1, Alias1),
	    add_alias(Alias1, Reg, Reg1, Alias2)
	), !,
	aliases1(LCode, Alias2, Alias3).




add_alias([], Reg, Reg1, [[Reg, Reg1]]).

add_alias([LSame|Alias], Reg, Reg1, [LSame1|Alias1]) :-
	(   set_elt(LSame, Reg) ->
	    set_add(LSame, Reg1, LSame1),
	    Alias1 = Alias
	;   LSame1 = LSame,
	    add_alias(Alias, Reg, Reg1, Alias1)
	).




find_aliases_of([LSame|Alias], Reg, LSame1) :-
	(   set_delete(LSame, Reg, LSame1) ->
	    true
	;   find_aliases_of(Alias, Reg, LSame1)
	).




remove_aliases_of([], _, []).

remove_aliases_of([LSame|Alias], Reg, Alias1) :-
	(   set_delete(LSame, Reg, LSame1) ->
	    (   (   LSame1 = []
	        ;   LSame1 = [_]
	        ) ->
	        Alias1 = Alias
	    ;   Alias1 = [LSame1|Alias]
	    )
	;   Alias1 = [LSame|Alias2],
	    remove_aliases_of(Alias, Reg, Alias2)
	).




          % Temporaries dictionnary creation (lifetime analysis)

create_lst_tmp([], [], [], []).

create_lst_tmp([InstW|LInstW], [Alias|LAlias], InLife1, LTmp1) :-
	create_lst_tmp(LInstW, LAlias, InLife, LTmp),
	codification(InstW, LCode), !,
	handle_lst_code(LCode, Alias, InLife, InLife1, LTmp, LTmp1).




handle_lst_code([], _, InLife, InLife, LTmp, LTmp).

handle_lst_code([Code|LCode], Alias, InLife, InLife2, LTmp, LTmp2) :-
	handle_lst_code(LCode, Alias, InLife, InLife1, LTmp, LTmp1),
	handle_one_code(Code, Alias, [], InLife1, InLife2, LTmp1, LTmp2).




handle_one_code(r(Reg), Alias, Wish, InLife, InLife1, LTmp, LTmp2) :-
	(   set_elt(InLife, Reg) ->
	    InLife1 = InLife,
	    (   var(Reg),
	        Wish \== [] ->
	        update_tmp(LTmp, Reg, [], Wish, LTmp2)
	    ;   LTmp2 = LTmp
	    )
	;   InLife1 = [Reg|InLife],
	    constraints(Reg, InLife, Alias, Cstr),
	    make_imposs(Cstr, [Reg], LTmp, LTmp1),
	    (   var(Reg) ->
	        update_tmp(LTmp1, Reg, Cstr, Wish, LTmp2)
	    ;   LTmp2 = LTmp1
	    )
	).

handle_one_code(w(Reg), Alias, Wish, InLife, InLife1, LTmp, LTmp2) :-
	(   set_delete(InLife, Reg, InLife1) ->
	    (   var(Reg),
	        Wish \== [] ->
	        update_tmp(LTmp, Reg, [], Wish, LTmp2)
	    ;   LTmp2 = LTmp
	    )
	;   InLife1 = InLife,
	    (   var(Reg) ->
	        constraints(Reg, InLife1, Alias, Cstr),
	        (   Wish \== [] ->
	            set_diff(Cstr, Wish, Cstr1)
	        ;   Cstr1 = Cstr
	        ),
	        make_imposs(Cstr1, [Reg], LTmp, LTmp1),
	        update_tmp(LTmp1, Reg, Cstr1, Wish, LTmp2)
	    ;   LTmp2 = LTmp
	    )
	).

handle_one_code(c(Reg, Reg1), Alias, _, InLife, InLife2, LTmp, LTmp2) :-
	handle_one_code(w(Reg1), Alias, [Reg], InLife, InLife1, LTmp, LTmp1),
	handle_one_code(r(Reg), Alias, [Reg1], InLife1, InLife2, LTmp1, LTmp2).




constraints(Reg, InLife, Alias, Cstr) :-
	(   g_read(reg_opt, 2),
	    find_aliases_of(Alias, Reg, LSame) ->
	    set_diff(InLife, LSame, Cstr)
	;   Cstr = InLife
	).




update_tmp([], Reg, Imposs, Wish, [tmp(Reg, Imposs, Wish)]).

update_tmp([Tmp|LTmp], Reg, Imposs, Wish, [Tmp1|LTmp1]) :-
	Tmp = tmp(Reg1, Imposs1, Wish1),
	(   Reg == Reg1 ->
	    set_union(Imposs, Imposs1, Imposs2),
	    set_union(Wish, Wish1, Wish2),
	    Tmp1 = tmp(Reg, Imposs2, Wish2),
	    LTmp1 = LTmp
	;   Tmp1 = Tmp,
	    update_tmp(LTmp, Reg, Imposs, Wish, LTmp1)
	).




remove_tmp([T|LTmp], Reg, Imposs, Wish, LTmp2) :-
	T = tmp(Reg1, Imposs1, Wish1),
	(   Reg == Reg1 ->
	    Imposs = Imposs1,
	    Wish = Wish1,
	    LTmp2 = LTmp
	;   LTmp2 = [T|LTmp1],
	    remove_tmp(LTmp, Reg, Imposs, Wish, LTmp1)
	).





make_imposs([], _, LTmp, LTmp).

make_imposs([Reg|Cstr], Imposs, LTmp, LTmp2) :-
	(   var(Reg) ->
	    update_tmp(LTmp, Reg, Imposs, [], LTmp1)
	;   LTmp1 = LTmp
	),
	make_imposs(Cstr, Imposs, LTmp1, LTmp2).




          % Register assignment

assign_lst_tmp(LTmp, MaxRegUsed) :-
	g_read(reg_opt, OptReg),
	(   OptReg = 2 ->
	    assign_wishes(LTmp, LTmp1)
	;   no_wish(LTmp, OptReg, LTmp1)
	),
	assign_values(LTmp1, -1, MaxRegUsed).




assign_wishes([], []).

assign_wishes([tmp(Tmp, Imposs, Wish)|LTmp], LTmp3) :-
	collapse_tmps(Wish, Imposs, LTmp, Tmp, Wish1, Imposs1, LTmp1),
	try_a_whish(Tmp, Imposs1, Wish1),
	(   var(Tmp) ->
	    LTmp3 = [tmp(Tmp, Imposs1)|LTmp2]       % no longer wish in tmp()
	;   LTmp3 = LTmp2
	),
	assign_wishes(LTmp1, LTmp2).




collapse_tmps([], Imposs, LTmp, _, [], Imposs, LTmp).

collapse_tmps([Reg|Wish], Imposs, LTmp, Tmp, Wish1, Imposs1, LTmp1) :-
	(   Reg == Tmp
	;   set_elt(Imposs, Reg)
	), !,
	collapse_tmps(Wish, Imposs, LTmp, Tmp, Wish1, Imposs1, LTmp1).

collapse_tmps([Arg|Wish], Imposs, LTmp, Tmp, [Arg|Wish1], Imposs1, LTmp1) :-
	integer(Arg), !,
	collapse_tmps(Wish, Imposs, LTmp, Tmp, Wish1, Imposs1, LTmp1).

collapse_tmps([Tmp1|Wish], Imposs, LTmp, Tmp, Wish3, Imposs3, LTmp2) :-
	remove_tmp(LTmp, Tmp1, Imposs1, Wish1, LTmp1),
	set_union(Imposs, Imposs1, Imposs2),
	set_union(Wish, Wish1, Wish2),
	Tmp = Tmp1,
	collapse_tmps(Wish2, Imposs2, LTmp1, Tmp, Wish3, Imposs3, LTmp2).




try_a_whish(Tmp, Imposs, Wish) :-
	set_diff(Wish, Imposs, [Tmp|_]), !.

try_a_whish(_, _, _).




no_wish([], _, []).

no_wish([tmp(Tmp, Imposs, Wish)|LTmp], OptReg, [tmp(Tmp, Imposs1)|LTmp1]) :-
	(   OptReg = 0 ->
	    set_union(Imposs, Wish, Imposs1)        % no optimizations at all
	;   Imposs1 = Imposs
	),                                           % for some optimizations
	no_wish(LTmp, OptReg, LTmp1).




assign_values([], MaxRegUsed, MaxRegUsed).

assign_values([tmp(Tmp, Imposs)|LTmp], MaxRegUsed, MaxRegUsed2) :-
	sort(Imposs, Imposs1),
	find_hole(Imposs1, 0, Tmp),
	(   Tmp > MaxRegUsed ->
	    MaxRegUsed1 = Tmp
	;   MaxRegUsed1 = MaxRegUsed
	),
	assign_values(LTmp, MaxRegUsed1, MaxRegUsed2).




find_hole([], Nb, Nb).

find_hole([Reg|Imposs], Nb, Nb1) :-
	var(Reg), !,
	find_hole(Imposs, Nb, Nb1).

find_hole([Reg|Imposs], Nb, Nb2) :-
	(   Reg > Nb ->
	    Nb2 = Nb                                             % hole found
	;   (   Reg == Nb ->
	        Nb1 is Nb + 1
	    ;   Nb1 = Nb
	    ),
	    find_hole(Imposs, Nb1, Nb2)
	).




          % Set handling (without unification)

set_add([], X, [X]).

set_add([Y|L], X, [Y|L]) :-
	X == Y, !.

set_add([Y|L], X, [Y|L1]) :-
	set_add(L, X, L1).




set_delete([Y|L], X, L) :-                      % set_delete(L,X,L1) fails if
	X == Y,                                      % X does not belong to L
	        !.

set_delete([Y|L], X, [Y|L1]) :-
	set_delete(L, X, L1).




set_elt([Y|_], X) :-
	X == Y, !.

set_elt([_|L], X) :-
	set_elt(L, X).




set_inter([], _, []).

set_inter([X|L1], L2, [X|L3]) :-
	set_elt(L2, X), !,
	set_inter(L1, L2, L3).

set_inter([_|L1], L2, L3) :-
	set_inter(L1, L2, L3).




set_union([], L2, L2).

set_union([X|L1], L2, L3) :-
	set_elt(L2, X), !,
	set_union(L1, L2, L3).

set_union([X|L1], L2, [X|L3]) :-
	set_union(L1, L2, L3).




set_diff([], _, []).

set_diff([X|L], L1, L3) :-
	(   set_elt(L1, X) ->
	    L3 = L2
	;   L3 = [X|L2]
	),
	set_diff(L, L1, L2).
