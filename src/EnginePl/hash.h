/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : hash.h                                                          *
 * Descr.: hash table management - header file                             *
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

typedef struct
{
  char *endt;
  char *cur_t;
  char *cur_p;
}
HashScan;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

char *Pl_Hash_Alloc_Table(int tbl_size, int elem_size);

void Pl_Hash_Free_Table(char *tbl);

char *Pl_Hash_Realloc_Table(char *tbl, int new_tbl_size);

void Pl_Hash_Delete_All(char *tbl);

char *Pl_Hash_Insert(char *tbl, char *elem, int replace);

char *Pl_Hash_Find(char *tbl, PlLong key);

char *Pl_Hash_Delete(char *tbl, PlLong key);

char *Pl_Hash_First(char *tbl, HashScan *scan);

char *Pl_Hash_Next(HashScan *scan);

int Pl_Hash_Table_Size(char *tbl);

int Pl_Hash_Nb_Elements(char *tbl);

#ifdef DEBUG

void Hash_Check_Table(char *tbl);

#endif
