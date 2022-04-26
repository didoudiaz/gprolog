/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : WAM to mini-assembler translator                                *
 * File  : bt_string.c                                                     *
 * Descr.: string dico management (file included by wam2ma.c and ma2asm.c) *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2002 Daniel Diaz                                     *
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

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct btnode *PBTNode;

typedef struct btnode
{
  char *str;
  int no;
  int arity;			/* FIXME: CROCK */
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

void BT_String_List(BTString *bt_str, void (*fct) ());



static BTNode *BT_String_Add_Rec(BTString *bt_str, BTNode **pbt_node,
				 char *str);

static void BT_String_List_Rec(BTNode *bt_node, void (*fct) ());




/*-------------------------------------------------------------------------*
 * BT_STRING_INIT                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
BT_String_Init(BTString *bt_str)
{
  bt_str->tree = NULL;
  bt_str->nb_elem = 0;
}




/*-------------------------------------------------------------------------*
 * BT_STRING_ADD                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
BTNode *
BT_String_Add(BTString *bt_str, char *str)
{
  return BT_String_Add_Rec(bt_str, &(bt_str->tree), str);
}




/*-------------------------------------------------------------------------*
 * BT_STRING_ADD_REC                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static BTNode *
BT_String_Add_Rec(BTString *bt_str, BTNode **pbt_node, char *str)
{
  BTNode *bt_node;
  int cmp;

  if (*pbt_node == NULL)
    {
      if ((bt_node = (BTNode *) malloc(sizeof(BTNode))) == NULL)
	{
	  fprintf(stderr, "Cannot allocate memory for BT string: %s\n",
		  str);
	  exit(1);
	}

      bt_node->str = str;
      bt_node->no = (bt_str->nb_elem)++;
      bt_node->left = bt_node->right = NULL;
      *pbt_node = bt_node;
      return bt_node;
    }

  bt_node = *pbt_node;
  cmp = strcmp(str, bt_node->str);

  if (cmp == 0)
    return bt_node;

  pbt_node = (cmp < 0) ? &(bt_node->left) : &(bt_node->right);

  return BT_String_Add_Rec(bt_str, pbt_node, str);
}




/*-------------------------------------------------------------------------*
 * BT_STRING_LIST                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
BT_String_List(BTString *bt_str, void (*fct) ())
{
  BT_String_List_Rec(bt_str->tree, fct);
}




/*-------------------------------------------------------------------------*
 * BT_STRING_LIST_REC                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
BT_String_List_Rec(BTNode *bt_node, void (*fct) ())
{
  if (bt_node == NULL)
    return;

  BT_String_List_Rec(bt_node->left, fct);
  (*fct) (bt_node->no, bt_node->str);
  BT_String_List_Rec(bt_node->right, fct);
}
