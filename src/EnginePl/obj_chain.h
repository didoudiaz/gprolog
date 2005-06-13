/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : obj_chain.h                                                     *
 * Descr.: object chaining management - header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2005 Daniel Diaz                                     *
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

/*
 * added __attribute__((used)) which is a request that eventhough a
 * static variable looks unused to gcc, it emits it anyway (similar to
 * __attribute__((unused)), the difference is that the new attribute not only
 * supresses the warning, but also ensures it is emitted).
 */

#ifndef __GNUC__
#define ATTR_TO_KEEP_UNREF_STATIC_VAR
#elif __GNUC__ >= 3 && __GNUC_MINOR__ >= 3
#define ATTR_TO_KEEP_UNREF_STATIC_VAR __attribute__ ((used))
#else
#define ATTR_TO_KEEP_UNREF_STATIC_VAR __attribute__ ((unused))
#endif

#ifdef OBJ_INIT

static void (OBJ_INIT) ();

#ifndef _MSC_VER

extern ObjChain *obj_chain_stop;

static ObjChain obj_chain_start ATTR_TO_KEEP_UNREF_STATIC_VAR =
  { OBJ_CHAIN_MAGIC_1, OBJ_CHAIN_MAGIC_2, &obj_chain_stop, OBJ_INIT };

static ObjChain *obj_chain_stop = &obj_chain_start;

#if 0 /* antoher way to force to keep the chain : a fct using obj_chain_start
         which references the initializer function (OBJ_INIT) */
#define CPP_CAT1(x, y)   x ## y
#define CPP_CAT(x, y)    CPP_CAT1(x, y)
CPP_CAT(OBJ_INIT,_Dummy)(void) { exit((int) &obj_chain_start); }
#endif

#else

#pragma data_seg(".INIT$b")

static long obj_chain_start = (long) OBJ_INIT;

#pragma data_seg()

#endif /* _MSC_VER */

#endif /* OBJ_INIT */
