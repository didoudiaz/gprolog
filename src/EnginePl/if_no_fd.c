/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : if_no_fd.c                                                      *
 * Descr.: FD interface for Prolog engine                                  *
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
  void (*copy_of_pl_fd_reset_solver) () = Pl_Dummy_Ptr(pl_fd_reset_solver);

  if (copy_of_pl_fd_reset_solver == NULL)	/* FD solver not linked */
    return;

  (*copy_of_pl_fd_reset_solver) ();
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
