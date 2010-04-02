/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : WAM to mini-assembler translator                                *
 * File  : bt_string.h                                                     *
 * Descr.: string dico management - header file                            *
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

typedef struct btnode *PBTNode;

typedef struct btnode
{
  char *str;
  int no;
  char info[32];		/* a buffer to store some information */
  PBTNode left;
  PBTNode right;
}
BTNode;




typedef struct
{
  BTNode *tree;
  int nb_elem;
}
BTString;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void BT_String_Init(BTString *bt_str);

BTNode *BT_String_Add(BTString *bt_str, char *str);

BTNode *BT_String_Lookup(BTString *bt_str, char *str);

void BT_String_List(BTString *bt_str, void (*fct) ());
