/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : if_no_fd.h                                                      *
 * Descr.: FD interface for Prolog engine - header file                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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


void (*pl_fd_init_solver) (void);	/* overwritten by FD if present */
void (*pl_fd_reset_solver) (void);

Bool (*pl_fd_unify_with_integer) (WamWord *fdv_adr, int n);
Bool (*pl_fd_unify_with_fd_var) (WamWord *fdv_adr1, WamWord *fdv_adr2);
int (*pl_fd_variable_size) (WamWord *fdv_adr);
int (*pl_fd_copy_variable) (WamWord *dst_adr, WamWord *fdv_adr);
char *(*pl_fd_variable_to_string) (WamWord *fdv_adr);

#else

extern void (*pl_fd_init_solver) (void);
extern void (*pl_fd_reset_solver) (void);
extern Bool (*pl_fd_unify_with_integer) (WamWord *fdv_adr, int n);
extern Bool (*pl_fd_unify_with_fd_var) (WamWord *fdv_adr1, WamWord *fdv_adr2);
extern int (*pl_fd_variable_size) (WamWord *fdv_adr);
extern int (*pl_fd_copy_variable) (WamWord *dst_adr, WamWord *fdv_adr);
extern char *(*pl_fd_variable_to_string) (WamWord *fdv_adr);

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
