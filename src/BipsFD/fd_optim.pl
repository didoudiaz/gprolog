/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_optim.pl                                                     *
 * Descr.: optimization predicate management                               *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2012 Daniel Diaz                                     *
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

/* $Id$ */

:-	built_in_fd.

'$use_fd_optim'.


:- meta_predicate(fd_minimize(0, ?)).
:- meta_predicate(fd_maximize(0, ?)).

fd_minimize(Goal, Var) :-
	fd_max_integer(Inf),
	g_assign('$cur_min', Inf),
	repeat,
	g_read('$cur_min', B),
	B1 is B - 1,
	set_bip_name(fd_minimize, 2),
	(   '$fd_domain'(Var, 0, B1),
	    '$call'(Goal, user, fd_minimize, 2) -> % FIXME: CallerModule
	    fd_min(Var, C),
	    g_assign('$cur_min', C),
	    fail
	;   !,
	    Var = B,
	    '$call'(Goal, user, fd_minimize, 2) % FIXME: CallerModule
	).




fd_maximize(Goal, Var) :-
	fd_max_integer(Inf),
	g_assign('$cur_max', 0),
	repeat,
	g_read('$cur_max', B),
	B1 is B + 1,
	set_bip_name(fd_maximize, 2),
	(   '$fd_domain'(Var, B1, Inf),
	    '$call'(Goal, user, fd_maximize, 2) -> % FIXME: CallerModule
	    fd_max(Var, C),
	    g_assign('$cur_max', C),
	    fail
	;   !,
	    Var = B,
	    '$call'(Goal, user, fd_maximize, 2) % FIXME: CallerModule
	).
