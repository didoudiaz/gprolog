/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : FD constraint solver buit-in predicates                         * 
 * File  : fd_prime.pl                                                     * 
 * Descr.: prime constraint management                                     * 
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

'$use_fd_prime'.


fd_prime(X) :-
	set_bip_name(fd_prime, 1),
	fd_tell(prime_x(X)).




fd_not_prime(X) :-
	set_bip_name(fd_not_prime, 1),
	fd_tell(not_prime_x(X)).
