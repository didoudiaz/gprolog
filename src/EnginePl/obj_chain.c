/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : obj_chain.c                                                     *
 * Descr.: object chaining management                                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2009 Daniel Diaz                                     *
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

#include <stdio.h>

#include "pl_params.h"
#include "arch_dep.h"
#include "gp_config.h"
#include "obj_chain.h"

#define DBGPRINTF printf

#if 0
#define DEBUG
#endif


void Pl_Fatal_Error(char *format, ...);




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  void (*fct_obj_init) ();
  void (*fct_exec_system) ();
  void (*fct_exec_user) ();
}
ObjInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static ObjInf obj_tbl[MAX_OBJECT];
static int nb_obj = 0;



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/



/*-------------------------------------------------------------------------*
 * Under WIN32 with MSVC++ 6.0                                             *
 *                                                                         *
 * We use the possibility to define user sections in objects. We group the *
 * address of each initialization function in a same section (named .GPLC).*
 * We use 2 markers: obj_chain_begin and obj_chain_end whose address (&)   *
 * delimit the start and the end of the initializer function address table.*
 * To know the start and end address in this section we cannot assume the  *
 * (MS) linker fits the section in the order it finds the objects. However,*
 * if a section name contains a $ in it, the $ and all everything that     *
 * follows will be stripped off in the executable file. Before the linker  *
 * strips down the name, it combines the sections with names that match up *
 * to the $. The name portion after the $ is used in arranging the OBJ     *
 * sections in the executable. These sections are sorted alphabetically,   *
 * based on the portion of the name after the $. For example, 3 sections   *
 * called .GPLC$m, .GPLC$a and .GPLC$z will be combined into a single      *
 * section called .GPLC in the executable. The data in this section will   *
 * start with .GPLC$a's data, continue with .GPLC$m's data and end with    *
 * .GPLC$z's data. Inside a same (sub)section the order is unknown.        *
 * We use $a for obj_chain_begin, $z for obj_chain_end, $m for initializers*
 * WARNING: when linking do not use any superflous flag (e.g. debugging),  *
 * I have spent a lot of time to find that /ZI causes troubles (the .GPLC  *
 * section contains much more information and then it is not correct to use*
 * its whole content between &obj_chain_begin and &obj_begin_stop).        *
 *-------------------------------------------------------------------------*/

#ifdef _MSC_VER

#pragma data_seg(".GPLC$a")

static long obj_chain_begin = 1;

#pragma data_seg(".GPLC$z")

static long obj_chain_end = 1;

#pragma data_seg()

static void Accumulate_Objects(void);
#endif

#ifndef OBJ_CHAIN_REVERSE_ORDER
#define FOR_EACH_OBJ   for(i = 0; i < nb_obj; i++)
#else
#define FOR_EACH_OBJ   for(i = nb_obj; --i >= 0; )
#endif

/*-------------------------------------------------------------------------*
 * PL_FIND_LINKED_OBJECTS                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Find_Linked_Objects(void)
{
  int i;

#ifdef _MSC_VER
  Accumulate_Objects();
#endif

  FOR_EACH_OBJ			/* call Obj Init functions */
    {
      if (obj_tbl[i].fct_obj_init != NULL) {
#ifdef DEBUG
	DBGPRINTF("\n+++ Executing Obj Init Function at: %p\n",
		  (obj_tbl[i].fct_obj_init));
#endif
	(*(obj_tbl[i].fct_obj_init)) ();
      }
    }
  FOR_EACH_OBJ			/* call Exec System functions */
    {
      if (obj_tbl[i].fct_exec_system != NULL) {
#ifdef DEBUG
	DBGPRINTF("\n+++ Executing Exec System Function at: %p\n",
		  (obj_tbl[i].fct_exec_system));
#endif
	(*(obj_tbl[i].fct_exec_system)) ();
      }
    }

  FOR_EACH_OBJ			/* call Exec User functions */
    {
      if (obj_tbl[i].fct_exec_user != NULL) {
#ifdef DEBUG
	DBGPRINTF("\n+++ Executing Exec User Function at: %p\n",
		  (obj_tbl[i].fct_exec_user));
#endif
	(*(obj_tbl[i].fct_exec_user)) ();
      }
    }
}



/*-------------------------------------------------------------------------*
 * ACCUMULATE_OBJECTS                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
#ifdef _MSC_VER
static void
Accumulate_Objects(void) {
  long *p;
  void (*q) ();

#ifdef DEBUG
  DBGPRINTF("ObjChain: chain begin: %p\n", &obj_chain_begin);
  DBGPRINTF("ObjChain: chain end  : %p\n", &obj_chain_end);
#endif

  p = &obj_chain_begin;
  while (++p < &obj_chain_end)
    {
      q = (void (*)()) *p;
      if (q) {
#ifdef DEBUG
	DBGPRINTF("\n*** Obj Found  addr: %p  Initializer: %p\n", p, q);
#endif
	(*q) ();
      }
    }
}
#endif /* MSC_VER */




/*-------------------------------------------------------------------------*
 * PL_NEW_OBJECT                                                           *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
void
Pl_New_Object(void (*fct_obj_init)(), void (*fct_exec_system) (), void (*fct_exec_user) ())
{
  obj_tbl[nb_obj].fct_obj_init = fct_obj_init;
  obj_tbl[nb_obj].fct_exec_system = fct_exec_system;
  obj_tbl[nb_obj].fct_exec_user = fct_exec_user;
  nb_obj++;
}
