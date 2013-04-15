/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : WAM to mini-assembler translator                                *
 * File  : bt_string.h                                                     *
 * Descr.: string dico management - header file                            *
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
#if WORD_SIZE == 64
  int filler;		/* to preserve 64-bits align for info=PlLong (to avoid a SIGBUS) */
#endif
  char info[32];	/* a buffer to store some information */
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

typedef void (*BTStrLstFct) (int no, char *str, void *info);


/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void BT_String_Init(BTString *bt_str);

BTNode *BT_String_Add(BTString *bt_str, char *str);

BTNode *BT_String_Lookup(BTString *bt_str, char *str);

void BT_String_List(BTString *bt_str, BTStrLstFct fct);
