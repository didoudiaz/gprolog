/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : FD constraint solver buit-in predicates                         * 
 * File  : fd_optim.pl                                                     * 
 * Descr.: optimization predicate management                               * 
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

'$use_fd_optim'.


fd_minimize(Goal, Var) :-
	fd_max_integer(Inf),
	g_assign('$cur_min', Inf),
	repeat,
	g_read('$cur_min', B),
	B1 is B - 1,
	set_bip_name(fd_minimize, 2),
	(   '$fd_domain'(Var, 0, B1),
	    '$call'(Goal, fd_minimize, 2, true) ->
	    fd_min(Var, C),
	    g_assign('$cur_min', C),
	    fail
	;   !,
	    Var = B,
	    '$call'(Goal, fd_minimize, 2, true)
	).




fd_maximize(Goal, Var) :-
	fd_max_integer(Inf),
	g_assign('$cur_max', 0),
	repeat,
	g_read('$cur_max', B),
	B1 is B + 1,
	set_bip_name(fd_maximize, 2),
	(   '$fd_domain'(Var, B1, Inf),
	    '$call'(Goal, fd_maximize, 2, true) ->
	    fd_max(Var, C),
	    g_assign('$cur_max', C),
	    fail
	;   !,
	    Var = B,
	    '$call'(Goal, fd_maximize, 2, true)
	).
