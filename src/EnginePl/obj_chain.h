/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : obj_chain.h                                                     *
 * Descr.: object chaining management - header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
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

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define OBJ_CHAIN_MAGIC_1          0xdeadbeef
#define OBJ_CHAIN_MAGIC_2          0x12345678




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

#ifndef _MSC_VER

typedef struct objchain *PObjChain;

typedef struct objchain
{
  long magic1;
  long magic2;
  PObjChain *next;
  void (*fct_init) ();
}
ObjChain;

#endif




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Find_Linked_Objects(void);

void New_Object(void (*fct_exec_system) (), void (*fct_exec_user) ());



#ifdef OBJ_INIT

static void (OBJ_INIT) ();

#ifndef _MSC_VER

extern ObjChain *obj_chain_stop;

static ObjChain obj_chain_start =
  { OBJ_CHAIN_MAGIC_1, OBJ_CHAIN_MAGIC_2, &obj_chain_stop, OBJ_INIT };

static ObjChain *obj_chain_stop = &obj_chain_start;

#else

#pragma data_seg(".INIT$b")

static long obj_chain_start = (long) OBJ_INIT;

#pragma data_seg()

#endif /* _MSC_VER */

#endif /* OBJ_INIT */
