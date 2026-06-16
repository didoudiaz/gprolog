/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : unit_ctor.c                                                     *
 * Descr.: Prolog unit initialization                                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2026 Daniel Diaz                                     *
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

#include "gp_config.h" 

#include <stdio.h>

#include "pl_params.h"
#include "unit_ctor.h"

#if 0
#define DEBUG
#endif

void Pl_Fatal_Error(char *format, ...); /* from mish.h to be minimal */



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  void (*fct_unit_init) ();
  void (*fct_exec_system) ();
  void (*fct_exec_user) ();
}
UnitInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static UnitInf unit_tbl[MAX_UNIT];
static int nb_unit = 0;



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

/*
 * A Prolog unit is a Prolog program compiled to native code.
 *
 * Each Prolog source file (.pl) is compiled into a native object file
 * (.o). For each object, a constructor is generated (see ctor.h).
 * This constructor is executed at program startup, before main(),
 * and calls Pl_Register_Unit() to register the unit in a global table.
 *
 * The register phase only records the presence of all linked Prolog units.
 * No Prolog data structures are created at this stage, since the Prolog
 * engine is not initialized yet.
 *
 * Once the Prolog engine is ready, Pl_Initialize_Units() is called.
 * It iterates over the table of registered units and initializes them
 * (record atoms, predicates, execute directives).
 *
 * Note for MSVC/Win32: Before 1.6.0 we used specific COFF/PE sections
 * (named .GPLC$<letter>). Now we use a unique ctor mecanism (see ctor.h)
 */

#ifndef UNIT_CHAIN_REVERSE_ORDER
#define FOR_EACH_UNIT_FROM_LAST_TO_FIRST   for(i = 0; i < nb_unit; i++)
#define FOR_EACH_UNIT_FROM_FIRST_TO_LAST   for(i = nb_unit; --i >= 0; )
#else
#define FOR_EACH_UNIT_FROM_LAST_TO_FIRST   for(i = nb_unit; --i >= 0; )
#define FOR_EACH_UNIT_FROM_FIRST_TO_LAST   for(i = 0; i < nb_unit; i++)
#endif


/*-------------------------------------------------------------------------*
 * PL_INITIALIZE_UNITS                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Initialize_Units(void)
{
  int i;

  FOR_EACH_UNIT_FROM_LAST_TO_FIRST /* call Unit Init functions */
    {
      if (unit_tbl[i].fct_unit_init != NULL) {
#ifdef DEBUG
	DBGPRINTF("\n+++ Executing Unit Init Function at: %p\n",
		  (unit_tbl[i].fct_unit_init));
#endif
	(*(unit_tbl[i].fct_unit_init)) ();
      }
    }
  FOR_EACH_UNIT_FROM_FIRST_TO_LAST /* call Exec System functions */
    {
      if (unit_tbl[i].fct_exec_system != NULL) {
#ifdef DEBUG
	DBGPRINTF("\n+++ Executing Exec System Function at: %p\n",
		  (unit_tbl[i].fct_exec_system));
#endif
	(*(unit_tbl[i].fct_exec_system)) ();
      }
    }

  FOR_EACH_UNIT_FROM_LAST_TO_FIRST /* call Exec User functions */
    {
      if (unit_tbl[i].fct_exec_user != NULL) {
#ifdef DEBUG
	DBGPRINTF("\n+++ Executing Exec User Function at: %p\n",
		  (unit_tbl[i].fct_exec_user));
#endif
	(*(unit_tbl[i].fct_exec_user)) ();
      }
    }
}




/*-------------------------------------------------------------------------*
 * PL_REGISTER_UNIT                                                        *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Register_Unit(void (*fct_unit_init)(), void (*fct_exec_system) (),
		 void (*fct_exec_user) ())
{
#ifdef DEBUG
  DBGPRINTF("\n--> Pl_Register_Unit  unit_init:%p  exec_sys:%p   exec_user:%p\n",
            fct_unit_init, fct_exec_system, fct_exec_user);
#endif
  unit_tbl[nb_unit].fct_unit_init = fct_unit_init;
  unit_tbl[nb_unit].fct_exec_system = fct_exec_system;
  unit_tbl[nb_unit].fct_exec_user = fct_exec_user;
  nb_unit++;
}
