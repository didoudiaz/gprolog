/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : WAM to mini-assembler translator                                *
 * File  : bt_string.c                                                     *
 * Descr.: string dico management (file included by wam2ma.c and ma2asm.c) *
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


#include "bt_string.h"

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

static void BT_String_List_Rec(BTNode *bt_node, BTStrLstFct fct);




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
  BTNode **pbt_node = &bt_str->tree;
  BTNode *bt_node;
  int cmp;

  while(*pbt_node)
    {
      bt_node = *pbt_node;
      cmp = strcmp(str, bt_node->str);

      if (cmp == 0)
	return bt_node;

      pbt_node = (cmp < 0) ? &(bt_node->left) : &(bt_node->right);
    }

  if ((bt_node = (BTNode *) malloc(sizeof(BTNode))) == NULL)
    {
      fprintf(stderr, "Cannot allocate memory for BT string: %s\n",
	      str);
      exit(1);
    }

  bt_node->str = str;
  bt_node->no = (bt_str->nb_elem)++;
  memset(bt_node->info, 0, sizeof(bt_node)->info);
  bt_node->left = bt_node->right = NULL;
  *pbt_node = bt_node;
  return bt_node;
}




/*-------------------------------------------------------------------------*
 * BT_STRING_LOOKUP                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
BTNode *
BT_String_Lookup(BTString *bt_str, char *str)
{
  BTNode *bt_node = bt_str->tree;
  int cmp;

  while(bt_node)
    {
      cmp = strcmp(str, bt_node->str);
      if (cmp == 0)
	return bt_node;
      bt_node =  (cmp < 0) ? bt_node->left : bt_node->right;
    }
  return NULL;
}




/*-------------------------------------------------------------------------*
 * BT_STRING_LIST                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
BT_String_List(BTString *bt_str, BTStrLstFct fct)
{
  BT_String_List_Rec(bt_str->tree, fct);
}




/*-------------------------------------------------------------------------*
 * BT_STRING_LIST_REC                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
BT_String_List_Rec(BTNode *bt_node, BTStrLstFct fct)
{
  if (bt_node == NULL)
    return;

  BT_String_List_Rec(bt_node->left, fct);
  (*fct) (bt_node->no, bt_node->str, (void *) bt_node->info);
  BT_String_List_Rec(bt_node->right, fct);
}
