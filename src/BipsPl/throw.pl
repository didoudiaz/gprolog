/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : throw.pl                                                        * 
 * Descr.: exception management (throw)                                    * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2001 Daniel Diaz                                     * 
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

:-	built_in.

'$use_throw'.


'$throw'(Ball, Func, Arity, DebugCall) :-
	'$call_c'('Save_Call_Info_3'(Func, Arity, DebugCall)),
	'$throw1'(Ball, 0).


'$throw1'(Ball, CallInfo) :-
	'$call_c'('Load_Call_Info_Arg_1'(1)),   % to ensure CallInfo is deref
	'$throw_internal'(Ball, CallInfo).


'$throw_internal'(Ball, CallInfo) :-
	(   var(Ball) ->
	    '$call_c'('Call_Info_Bip_Name_1'(CallInfo)),
	    '$pl_err_instantiation'
	;   true
	),
	'$sys_var_put'(8, Ball),
	'$unwind'(Ball).




'$unwind'(Ball) :-
	'$sys_var_read'(7, Handler),
	'$call_c'('Throw_2'(Ball, Handler)),              % mainly does a cut
	fail.
