/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : hash.h                                                          *
 * Descr.: hash table management - header file                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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

char *Hash_Alloc_Table(int tbl_size, int elem_size);

void Hash_Free_Table(char *tbl);

char *Hash_Realloc_Table(char *tbl, int new_tbl_size);

void Hash_Delete_All(char *tbl);

char *Hash_Insert(char *tbl, char *elem, int replace);

char *Hash_Find(char *tbl, long key);

char *Hash_Delete(char *tbl, long key);

char *Hash_First(char *tbl, HashScan *scan);

char *Hash_Next(HashScan *scan);

int Hash_Table_Size(char *tbl);

int Hash_Nb_Elements(char *tbl);

#ifdef DEBUG

void Hash_Check_Table(char *tbl);

#endif
