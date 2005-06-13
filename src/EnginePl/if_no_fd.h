/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : if_no_fd.h                                                      *
 * Descr.: FD interface for Prolog engine - header file                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2005 Daniel Diaz                                     *
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

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef IF_NO_FD_FILE

void (*fd_init_solver) ();	/* overwritten by FD if present */
void (*fd_reset_solver) ();

Bool (*fd_unify_with_integer) ();
Bool (*fd_unify_with_fd_var) ();
int (*fd_variable_size) ();
int (*fd_copy_variable) ();
char *(*fd_variable_to_string) ();

#else

extern void (*fd_init_solver) ();
extern void (*fd_reset_solver) ();
extern Bool (*fd_unify_with_integer) ();
extern Bool (*fd_unify_with_fd_var) ();
extern int (*fd_variable_size) ();
extern int (*fd_copy_variable) ();
extern char *(*fd_variable_to_string) ();

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Fd_Init_Solver(void);

void Fd_Reset_Solver(void);

#define Fd_Unify_With_Integer(f, n) ((*fd_unify_with_integer)(f, n))

#define Fd_Unify_With_Fd_Var(f1, f2)((*fd_unify_with_fd_var)(f1, f2))

#define Fd_Variable_Size(f)         ((*fd_variable_size)(f))

#define Fd_Copy_Variable(dst_adr, f)((*fd_copy_variable)(dst_adr, f))

#define Fd_Variable_To_String(f)    ((*fd_variable_to_string)(f))
