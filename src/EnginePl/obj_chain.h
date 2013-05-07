/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : obj_chain.h                                                     *
 * Descr.: object chaining management - header file                        *
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


#include "gp_config.h"
#include "pl_long.h"

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

void Pl_Find_Linked_Objects(void);

void Pl_New_Object(void (*fct_obj_init)(), void (*fct_exec_system) (), void (*fct_exec_user) ());


#ifdef OBJ_INIT

static void OBJ_INIT(void);

#define OBJ_CTOR  CPP_CAT(OBJ_INIT,_ctor)


#ifdef __GNUC__
static void __attribute__ ((constructor))
OBJ_CTOR(void)
{
  Pl_New_Object(OBJ_INIT, NULL, NULL);
}


#else /* _MSC_VER */

static void
OBJ_CTOR(void)
{
  Pl_New_Object(OBJ_INIT, NULL, NULL);
}

#pragma data_seg(".GPLC$m")

static PlLong obj_chain_start = (PlLong) OBJ_CTOR;

#pragma data_seg()

#endif /* _MSC_VER */

#endif /* OBJ_INIT */



#if (defined(_MSC_VER) || defined(M_darwin)) && !defined(OBJ_CHAIN_REVERSE_ORDER)
#define OBJ_CHAIN_REVERSE_ORDER
#endif

#if 0
#define OBJ_CHAIN_REVERSE_ORDER
#endif

