/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : WAM to mini-assembler translator                                *
 * File  : wam_protos.h                                                    *
 * Descr.: code generation - header file                                   *
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

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Source_Line(int line_no, char *cmt);

void Prolog_File_Name(char *pl_file);

void New_Predicate(char *functor, int arity, int pl_line,
		   int dynamic, int public, int built_in, int built_in_fd);

void New_Directive(int pl_line, int system);

void Ensure_Linked(ArgVal arg[]);



void F_get_variable(ArgVal arg[]);

void F_get_value(ArgVal arg[]);

void F_get_atom(ArgVal arg[]);

void F_get_integer(ArgVal arg[]);

void F_get_float(ArgVal arg[]);

void F_get_nil(ArgVal arg[]);

void F_get_list(ArgVal arg[]);

void F_get_structure(ArgVal arg[]);

void F_put_variable(ArgVal arg[]);

void F_put_void(ArgVal arg[]);

void F_put_value(ArgVal arg[]);

void F_put_unsafe_value(ArgVal arg[]);

void F_put_atom(ArgVal arg[]);

void F_put_integer(ArgVal arg[]);

void F_put_float(ArgVal arg[]);

void F_put_nil(ArgVal arg[]);

void F_put_list(ArgVal arg[]);

void F_put_structure(ArgVal arg[]);

void F_math_load_value(ArgVal arg[]);

void F_math_fast_load_value(ArgVal arg[]);

void F_unify_variable(ArgVal arg[]);

void F_unify_void(ArgVal arg[]);

void F_unify_value(ArgVal arg[]);

void F_unify_local_value(ArgVal arg[]);

void F_unify_atom(ArgVal arg[]);

void F_unify_integer(ArgVal arg[]);

void F_unify_nil(ArgVal arg[]);

void F_unify_list(ArgVal arg[]);

void F_unify_structure(ArgVal arg[]);

void F_allocate(ArgVal arg[]);

void F_deallocate(ArgVal arg[]);

void F_call(ArgVal arg[]);

void F_execute(ArgVal arg[]);

void F_proceed(ArgVal arg[]);

void F_fail(ArgVal arg[]);

void F_label(ArgVal arg[]);

void F_switch_on_term(ArgVal arg[]);

void F_switch_on_atom(ArgVal arg[]);

void F_switch_on_integer(ArgVal arg[]);

void F_switch_on_structure(ArgVal arg[]);

void F_try_me_else(ArgVal arg[]);

void F_retry_me_else(ArgVal arg[]);

void F_trust_me_else_fail(ArgVal arg[]);

void F_try(ArgVal arg[]);

void F_retry(ArgVal arg[]);

void F_trust(ArgVal arg[]);

void F_load_cut_level(ArgVal arg[]);

void F_cut(ArgVal arg[]);

void F_function(ArgVal arg[]);

void F_call_c(ArgVal arg[]);

void F_foreign_call_c(ArgVal arg[]);
