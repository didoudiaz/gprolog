/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : call_args.pl                                                    * 
 * Descr.: meta call management                                            * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999,2000 Daniel Diaz                                     * 
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

:-	built_in.

'$use_call_args'.




call_with_args(_F) :-
	'$call_c_jump'('Call_With_Args_1').

call_with_args(_F, _A1) :-
	'$call_c_jump'('Call_With_Args_2').

call_with_args(_F, _A1, _A2) :-
	'$call_c_jump'('Call_With_Args_3').

call_with_args(_F, _A1, _A2, _A3) :-
	'$call_c_jump'('Call_With_Args_4').

call_with_args(_F, _A1, _A2, _A3, _A4) :-
	'$call_c_jump'('Call_With_Args_5').

call_with_args(_F, _A1, _A2, _A3, _A4, _A5) :-
	'$call_c_jump'('Call_With_Args_6').

call_with_args(_F, _A1, _A2, _A3, _A4, _A5, _A6) :-
	'$call_c_jump'('Call_With_Args_7').

call_with_args(_F, _A1, _A2, _A3, _A4, _A5, _A6, _A7) :-
	'$call_c_jump'('Call_With_Args_8').

call_with_args(_F, _A1, _A2, _A3, _A4, _A5, _A6, _A7, _A8) :-
	'$call_c_jump'('Call_With_Args_9').

call_with_args(_F, _A1, _A2, _A3, _A4, _A5, _A6, _A7, _A8, _A9) :-
	'$call_c_jump'('Call_With_Args_10').

call_with_args(_F, _A1, _A2, _A3, _A4, _A5, _A6, _A7, _A8, _A9, _A10) :-
	'$call_c_jump'('Call_With_Args_11').

