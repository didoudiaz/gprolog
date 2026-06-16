/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : hash_tbl.h                                                      *
 * Descr.: hash table management - header file                             *
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


#ifndef _HASH_TBL_H
#define _HASH_TBL_H

#include "pl_long.h"
#include "bool.h"



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/


/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct hash_node *HashNode;

struct hash_node
{
  HashNode next;
  PlLong key;
  /* the rest of the user elem comes here */
};



typedef struct
{
  HashNode *end_t;
  HashNode *cur_t;
  HashNode cur_p;
}
HashScan;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void *Pl_HTBL_Alloc_Table(int tbl_size, int elem_size);

void Pl_HTBL_Free_Table(void *htbl);

void *Pl_HTBL_Realloc_Table(void *htbl, int new_tbl_size);

void Pl_HTBL_Delete_All(void *htbl);

void *Pl_HTBL_Insert(void *htbl, void *elem, Bool replace);

void *Pl_HTBL_Find(void *htbl, PlLong key);

void *Pl_HTBL_Delete(void *htbl, PlLong key);

void *Pl_HTBL_First(void *htbl, HashScan *scan);

void *Pl_HTBL_Next(HashScan *scan);

int Pl_HTBL_Table_Size(void *htbl);

int Pl_HTBL_Nb_Elements(void *htbl);

#endif	/* !_HASH_TBL_H */
