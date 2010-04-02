/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : obj_chain.h                                                     *
 * Descr.: object chaining management - header file                        *
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

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Pl_Find_Linked_Objects(void);

void Pl_New_Object(void (*fct_obj_init)(), void (*fct_exec_system) (), void (*fct_exec_user) ());


#ifdef OBJ_INIT

static void OBJ_INIT(void);

#define CPP_CAT1(x, y)   x ## y
#define CPP_CAT(x, y)    CPP_CAT1(x, y)

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

static long obj_chain_start = (long) OBJ_CTOR;

#pragma data_seg()

#endif /* _MSC_VER */

#endif /* OBJ_INIT */



#if defined(_MSC_VER) || defined(M_darwin)
#define OBJ_CHAIN_REVERSE_ORDER
#endif
