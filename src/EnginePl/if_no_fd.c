/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : if_no_fd.c                                                      *
 * Descr.: FD interface for Prolog engine                                  *
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

#include <stdio.h>

#define IF_NO_FD_FILE

#include "engine_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/


	  /* Errors Messages */

#define ERR_FD_SOLVER_MISSING      "fd_interf.c: FD Solver not linked"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Fd_Solver_Missing(void);




/*-------------------------------------------------------------------------*
 * FD_INIT_SOLVER                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fd_Init_Solver(void)
{
  if (fd_init_solver == NULL)	/* FD solver not linked */
    {
      fd_unify_with_integer = (Bool (*)()) Fd_Solver_Missing;
      fd_unify_with_fd_var = (Bool (*)()) Fd_Solver_Missing;
      fd_variable_size = (int (*)()) Fd_Solver_Missing;
      fd_copy_variable = (int (*)()) Fd_Solver_Missing;
      fd_variable_to_string = (char *(*)()) Fd_Solver_Missing;
      return;
    }

  (*fd_init_solver) ();
}




/*-------------------------------------------------------------------------*
 * FD_RESET_SOLVER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fd_Reset_Solver(void)
{
  if (fd_reset_solver == NULL)	/* FD solver not linked */
    return;

  (*fd_reset_solver) ();
}




/*-------------------------------------------------------------------------*
 * FD_SOLVER_MISSING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fd_Solver_Missing(void)
{
  Fatal_Error(ERR_FD_SOLVER_MISSING);
}
