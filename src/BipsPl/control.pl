/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : control.pl                                                      * 
 * Descr.: control management                                              * 
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

:-	built_in.

'$use_control'.




repeat.

repeat :-
	repeat.




abort :-
	'$call_c_jump'('Halt_If_No_Top_Level_1'(1)).


stop :-
	'$call_c_jump'('Halt_If_No_Top_Level_1'(0)).





halt :-
	halt(0).




halt(X) :-
	set_bip_name(halt, 1),
	'$call_c'('Halt_1'(X)).





for(I, L, U) :-
	set_bip_name(for, 3),
	'$call_c_test'('For_3'(I, L, U)).


'$for_alt' :-
	'$call_c'('For_Alt_0').
