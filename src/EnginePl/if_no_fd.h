/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : if_no_fd.h                                                      *
 * Descr.: FD interface for Prolog engine - header file                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2010 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU Lesser General Public License as published   *
 * by the Free Software Foundation; either version 3, or any later version.*
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU Lesser General Public License*
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

void (*pl_fd_init_solver) ();	/* overwritten by FD if present */
void (*pl_fd_reset_solver) ();

Bool (*pl_fd_unify_with_integer) ();
Bool (*pl_fd_unify_with_fd_var) ();
int (*pl_fd_variable_size) ();
int (*pl_fd_copy_variable) ();
char *(*pl_fd_variable_to_string) ();

#else

extern void (*pl_fd_init_solver) ();
extern void (*pl_fd_reset_solver) ();
extern Bool (*pl_fd_unify_with_integer) ();
extern Bool (*pl_fd_unify_with_fd_var) ();
extern int (*pl_fd_variable_size) ();
extern int (*pl_fd_copy_variable) ();
extern char *(*pl_fd_variable_to_string) ();

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Pl_Fd_Init_Solver(void);

void Pl_Fd_Reset_Solver(void);

#define Fd_Unify_With_Integer(f, n) ((*pl_fd_unify_with_integer)(f, n))

#define Fd_Unify_With_Fd_Var(f1, f2)((*pl_fd_unify_with_fd_var)(f1, f2))

#define Fd_Variable_Size(f)         ((*pl_fd_variable_size)(f))

#define Fd_Copy_Variable(dst_adr, f)((*pl_fd_copy_variable)(dst_adr, f))

#define Fd_Variable_To_String(f)    ((*pl_fd_variable_to_string)(f))
