/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : file.pl                                                         *
 * Descr.: file name management                                            *
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


:-	built_in.

'$use_file'.


absolute_file_name(Path1, Path2) :-
	set_bip_name(absolute_file_name, 2),
	'$call_c_test'('Pl_Absolute_File_Name_2'(Path1, Path2)).




is_absolute_file_name(Path) :-
	set_bip_name(is_absolute_file_name, 1),
	'$call_c_test'('Pl_Is_Absolute_File_Name_1'(Path)).




is_relative_file_name(Path) :-
	set_bip_name(is_relative_file_name, 1),
	'$call_c_test'('Pl_Is_Relative_File_Name_1'(Path)).




decompose_file_name(Path, Dir, Prefix, Suffix) :-
	set_bip_name(decompose_file_name, 4),
	'$call_c_test'('Pl_Decompose_File_Name_4'(Path, Dir, Prefix, Suffix)).




prolog_file_name(PlFile, PlFile1) :-
	set_bip_name(prolog_file_name, 2),
	'$call_c_test'('Pl_Prolog_File_Name_2'(PlFile, PlFile1)).
