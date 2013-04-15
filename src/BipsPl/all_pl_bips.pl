/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : all_pl_bips.pl                                                  *
 * Descr.: all bips (to force the linker)                                  *
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


'$use_all_pl_bips' :-
	'$use_control',
	'$use_call',
	'$use_call_args',
	'$use_catch',
	'$use_throw',
	'$use_unify',
	'$use_arith_inl',
	'$use_assert',
	'$use_all_solut',
	'$use_sort',
	'$use_list',
	'$use_stream',
	'$use_le_interf',
	'$use_file',
	'$use_char_io',
	'$use_read',
	'$use_write',
	'$use_print',
	'$use_const_io',
	'$use_oper',
	'$use_pred',
	'$use_atom',
	'$use_flag',
	'$use_term_inl',
	'$use_type_inl',
	'$use_g_var_inl',
	'$use_stat',
	'$use_dec10io',
	'$use_format',
	'$use_os_interf',
	'$use_expand',
	'$use_pretty',
	'$use_random',
	'$use_sockets',
 	'$use_src_rdr'.

