/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : FD constraint solver buit-in predicates                         * 
 * File  : all_fd_bips.pl                                                  * 
 * Descr.: all bips (to force the linker)                                  * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2009 Daniel Diaz                                     * 
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
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               * 
 *-------------------------------------------------------------------------*/

/* $Id$ */

'$use_all_fd_bips' :-
	'$use_fd_infos',
	'$use_fd_values',
	'$use_fd_math',
	'$use_fd_bool',
	'$use_fd_prime',
	'$use_fd_symbolic',
	'$use_fd_optim'.
