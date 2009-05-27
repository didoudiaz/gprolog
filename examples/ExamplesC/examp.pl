/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : foreign facility test                                           *
 * File  : examp.pl                                                        *
 * Descr.: test file                                                       *
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
 
:- foreign(first_occurrence(+string, +char, -positive)).

:- foreign(occurrence(+string, +char, -positive), [choice_size(1)]).

:- foreign(occurrence2(+string, +char, -positive), [choice_size(1)]).

:- foreign(char_ascii(?char, ?code)).

:- foreign(char_ascii2(?char, ?code)).

:- foreign(my_call(term)).

:- foreign(my_call2(term)).

:- foreign(all_op(term)).
