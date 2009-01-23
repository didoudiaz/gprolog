/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : hash.h                                                          *
 * Descr.: hash table management - header file                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2009 Daniel Diaz                                     *
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

char *Pl_Hash_Find(char *tbl, long key);

char *Pl_Hash_Delete(char *tbl, long key);

char *Pl_Hash_First(char *tbl, HashScan *scan);

char *Pl_Hash_Next(HashScan *scan);

int Pl_Hash_Table_Size(char *tbl);

int Pl_Hash_Nb_Elements(char *tbl);

#ifdef DEBUG

void Hash_Check_Table(char *tbl);

#endif
