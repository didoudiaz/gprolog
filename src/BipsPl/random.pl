/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : random.pl                                                       * 
 * Descr.: random number generator management                              * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2003 Daniel Diaz                                     * 
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

'$use_random'.



randomize :-
	'$call_c'('M_Randomize').




set_seed(Seed) :-
	set_bip_name(set_seed, 1),
	'$call_c'('Set_Seed_1'(Seed)).




get_seed(Seed) :-
	set_bip_name(get_seed, 1),
	'$call_c_test'('Get_Seed_1'(Seed)).




random(N) :-
	set_bip_name(random, 1),
	'$call_c'('Random_1'(N)).




random(L, U, N) :-
	set_bip_name(random, 3),
	'$call_c_test'('Random_3'(L, U, N)).
