/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : if_no_fd.c                                                      *
 * Descr.: FD interface for Prolog engine                                  *
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

#include <stdio.h>

#define IF_NO_FD_FILE

#include "engine_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/


	  /* Errors Messages */

#define ERR_FD_SOLVER_MISSING      __FILE__ ": FD Solver not linked"




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
 * PL_FD_INIT_SOLVER                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Init_Solver(void)
{
  if (pl_fd_init_solver == NULL)	/* FD solver not linked */
    {
      pl_fd_unify_with_integer = (Bool (*)()) Fd_Solver_Missing;
      pl_fd_unify_with_fd_var = (Bool (*)()) Fd_Solver_Missing;
      pl_fd_variable_size = (int (*)()) Fd_Solver_Missing;
      pl_fd_copy_variable = (int (*)()) Fd_Solver_Missing;
      pl_fd_variable_to_string = (char *(*)()) Fd_Solver_Missing;
      return;
    }

  (*pl_fd_init_solver) ();
}




/*-------------------------------------------------------------------------*
 * PL_FD_RESET_SOLVER                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Reset_Solver(void)
{
  if (pl_fd_reset_solver == NULL)	/* FD solver not linked */
    return;

  (*pl_fd_reset_solver) ();
}




/*-------------------------------------------------------------------------*
 * FD_SOLVER_MISSING                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fd_Solver_Missing(void)
{
  Pl_Fatal_Error(ERR_FD_SOLVER_MISSING);
}
