/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : hash_tbl.c                                                      *
 * Descr.: hash table management                                           *
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

#include "gp_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "hash_tbl.h"


#if 0
#define USE_ALONE
#endif


#ifndef USE_ALONE

#include "engine_pl.h"

#else

#define Malloc(size)       malloc(size)
#define Calloc(nb, size)   calloc(nb, size)
#define Realloc(ptr, size) realloc(ptr, size)
#define Free(ptr)          free(ptr)

#endif




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

static HashNode *Hash_Locate(HashNode *t, int tbl_size, PlLong key);



#define HASH_STATIC_SIZE           3

#define Tbl_Size(t)                (((int *)(t))[0])

#define Elem_Size(t)               (((int *)(t))[1])

#define Nb_Elem(t)                 (((int *)(t))[2])

#define The_Array(t)               (((HashNode *) (t)) + HASH_STATIC_SIZE)



#define Hash_Function(k, size)     ((uintptr_t) (k) % (size))




/*-------------------------------------------------------------------------*
 * A hash table consists of a header (tbl_size, elem_size, nb_elem) and an *
 * array of tbl_size pointers to nodes.                                    *
 * Each node records a pointer to the next node, and a user element whose  *
 * size is elem_size. Each element must begin with the key (a PlLong).     *
 *-------------------------------------------------------------------------*/


/*-------------------------------------------------------------------------*
 * PL_HTBL_ALLOC_TABLE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void *
Pl_HTBL_Alloc_Table(int tbl_size, int elem_size)
{
  void *htbl;

  htbl = Calloc(HASH_STATIC_SIZE + tbl_size, sizeof(HashNode));
#ifdef USE_ALONE
  if (htbl == NULL)
    return NULL;
#endif

  if (tbl_size < 1)
    tbl_size = 1;		/* at least one cell in a table */

  Tbl_Size(htbl) = tbl_size;
  Elem_Size(htbl) = elem_size;
  Nb_Elem(htbl) = 0;

  return htbl;
}




/*-------------------------------------------------------------------------*
 * PL_HTBL_REALLOC_TABLE                                                   *
 *                                                                         *
 * NB: user information is not moved elsewhere                             *
 *-------------------------------------------------------------------------*/
void *
Pl_HTBL_Realloc_Table(void *htbl, int new_tbl_size)
{
  int tbl_size = Tbl_Size(htbl);
  int elem_size = Elem_Size(htbl);
  HashNode *t = The_Array(htbl);
  HashNode *end_t = t + tbl_size;
  HashNode p, p1;
  HashNode *prev;
  void *new_htbl;
  HashNode *new_t;

  if ((new_htbl = Pl_HTBL_Alloc_Table(new_tbl_size, elem_size)) == NULL)
    return NULL;

  Nb_Elem(new_htbl) = Nb_Elem(htbl);

  new_t = The_Array(new_htbl);

  do
    {
      p = *t;
      while (p)
	{			/* here *prev == NULL */
	  prev = Hash_Locate(new_t, new_tbl_size, p->key);
	  p1 = p;
	  p = p->next;
	  *prev = p1;
	  p1->next = NULL;
	}
    }
  while (++t < end_t);

  Free(htbl);

  return new_htbl;
}




/*-------------------------------------------------------------------------*
 * PL_HTBL_FREE_TABLE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_HTBL_Free_Table(void *htbl)
{
  int tbl_size = Tbl_Size(htbl);
  HashNode *t = The_Array(htbl);
  HashNode *end_t = t + tbl_size;
  HashNode p, p1;

  do
    {
      p = *t;
      while (p)
	{
	  p1 = p;
	  p = p->next;
	  Free(p1);
	}
    }
  while (++t < end_t);

  Free(htbl);
}




/*-------------------------------------------------------------------------*
 * PL_HTBL_DELETE_ALL                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_HTBL_Delete_All(void *htbl)
{
  int tbl_size = Tbl_Size(htbl);
  HashNode *t = The_Array(htbl);
  HashNode *end_t = t + tbl_size;
  HashNode p, p1;

  do
    {
      p = *t;
      while (p)
	{
	  p1 = p;
	  p = p->next;
	  Free(p1);
	}

      *t = NULL;
    }
  while (++t < end_t);

  Nb_Elem(htbl) = 0;
}




/*-------------------------------------------------------------------------*
 * HASH_INSERT                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void *
Pl_HTBL_Insert(void *htbl, void *elem, Bool replace)
{
  int tbl_size = Tbl_Size(htbl);
  int elem_size = Elem_Size(htbl);
  HashNode *t = The_Array(htbl);
  PlLong key = *(PlLong *) elem;
  HashNode *prev;
  HashNode p;

  prev = Hash_Locate(t, tbl_size, key);
  p = *prev;

  if (p == NULL)		/* the key does not exist */
    {
      p = Malloc(sizeof(struct hash_node) - sizeof(PlLong) + elem_size);
#ifdef USE_ALONE
      if (p == NULL)
	return NULL;
#endif

      p->next = NULL;
      Nb_Elem(htbl)++;
      *prev = p;
    }
  else if (!replace)
    goto finish;

  memcpy((void *) &(p->key), elem, elem_size);

finish:
  return (void *) &(p->key);
}




/*-------------------------------------------------------------------------*
 * PL_HTBL_FIND                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void *
Pl_HTBL_Find(void *htbl, PlLong key)
{
  int tbl_size = Tbl_Size(htbl);
  HashNode *t = The_Array(htbl);
  HashNode *prev;
  HashNode p;

  prev = Hash_Locate(t, tbl_size, key);
  p = *prev;

  if (p == NULL)		/* the key does not exist */
    return NULL;

  return (void *) &(p->key);
}




/*-------------------------------------------------------------------------*
 * PL_HTBL_DELETE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void *
Pl_HTBL_Delete(void *htbl, PlLong key)
{
  int tbl_size = Tbl_Size(htbl);
  HashNode *t = The_Array(htbl);
  HashNode *prev;
  HashNode p;

  prev = Hash_Locate(t, tbl_size, key);
  p = *prev;

  if (p == NULL)		/* the key does not exist */
    return NULL;

  *prev = p->next;
  Free(p);
  Nb_Elem(htbl)--;

  return htbl;
}




/*-------------------------------------------------------------------------*
 * HASH_LOCATE                                                             *
 *                                                                         *
 * This function returns the address of the pointer to the node associated *
 * to the key (if the pointer is NULL the key is not in the table).        *
 *-------------------------------------------------------------------------*/
static HashNode *
Hash_Locate(HashNode *t, int tbl_size, PlLong key)
{
  int n = Hash_Function(key, tbl_size);
  HashNode p;

  t += n;

  for (p = *t; p; p = p->next)
    {
      if (p->key == key)
	break;

      t = &p->next;
    }

  return t;
}




/*-------------------------------------------------------------------------*
 * PL_HTBL_FIRST                                                           *
 *                                                                         *
 * Hash_First and Hash_Next make it possible to scan a hash table.         *
 * Example of use:                                                         *
 *                                                                         *
 *   HashScan scan;                                                        *
 *   void *buff_ptr;                                                       *
 *                                                                         *
 *   for(buff_ptr = Pl_HTBL_First(htbl, &scan); buff_ptr;                  *
 *       buff_ptr = Pl_HTBL_Next(&scan))                                   *
 *         Display_Element(buff_ptr);                                      *
 *-------------------------------------------------------------------------*/
void *
Pl_HTBL_First(void *htbl, HashScan *scan)
{
  int tbl_size = Tbl_Size(htbl);
  HashNode *t = The_Array(htbl);
  HashNode *end_t = t + tbl_size;

  scan->end_t = end_t;
  scan->cur_t = t;
  scan->cur_p = *t;

  return Pl_HTBL_Next(scan);
}




/*-------------------------------------------------------------------------*
 * PL_HTBL_NEXT                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void *
Pl_HTBL_Next(HashScan *scan)
{
  HashNode *t;
  HashNode *end_t;
  HashNode p;

  p = scan->cur_p;
  if (p)
    {
      scan->cur_p = p->next;
      return (void *) &(p->key);
    }

  t = scan->cur_t;
  end_t = scan->end_t;

  while (++t < end_t)
    {
      p = *t;
      if (p)
	{
	  scan->cur_t = t;
	  scan->cur_p = p->next;
	  return (void *) &(p->key);
	}
    }

  return NULL;
}




/*-------------------------------------------------------------------------*
 * PL_HTBL_TABLE_SIZE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_HTBL_Table_Size(void *htbl)
{
  return Tbl_Size(htbl);
}




/*-------------------------------------------------------------------------*
 * PL_HTBL_NB_ELEMENTS                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_HTBL_Nb_Elements(void *htbl)
{
  return Nb_Elem(htbl);
}




#ifdef USE_ALONE

/*-------------------------------------------------------------------------*
 * HASH_CHECK_TABLE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_Table(void *htbl)
{
  int tbl_size = Tbl_Size(htbl);
  HashNode *t = The_Array(htbl);
  HashNode *end_t = t + tbl_size;
  HashNode p;
  int i = 0;

  do
    {
      printf("Hash Code: %ld\n", (long) (t - The_Array(htbl)));

      for (p = *t; p; p = p->next, i++)
	printf("\tadr: %p  key:%" PL_FMT_d "\n", p, p->key);
    }
  while (++t < end_t);

  if (i != Nb_Elem(htbl))
    printf("Nb_Elem counter erroneous = %d insted of %d\n", Nb_Elem(htbl), i);
}


typedef struct
{
  PlLong key;
  int info1;
  int info2;
}
Elem;




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 * to compile alone active USE_ALONE and simply compile this file.         *
 *-------------------------------------------------------------------------*/
int
main(void)
{
  void *t;
  int size;
  PlLong key;
  Elem elem, *p;
  HashScan scan;
  int c;
  int i = -1;
  int k;

  setbuf(stdout, NULL);
  printf("initial size: ");
  scanf("%d", &size);
  getchar();
  t = Pl_HTBL_Alloc_Table(size, sizeof(Elem));
  if (t == NULL)
    printf("Cannot allocate the table\n");


  for (;;)
    {
      printf
	("\n1-Add/No Replace   2-Add/Replace   3-Find   4-Delete   5-DeleteAll\n");
      printf("6-Scan Table   7-Exend Table   8-Check Integrity    9-End:");
      scanf("%d", &c);
      getchar();

      if (c == 9)
	break;

      if (c <= 4)
	{
	  printf("Key:");
	  scanf("%" PL_FMT_d "", &key);
	  getchar();
	  elem.key = key;
	}

      i = -i;
      switch (c)
	{
	case 1:
	case 2:
	  elem.info1 = key * i * 10;
	  elem.info2 = key * i * 100;
	  printf("passed value: Key:%" PL_FMT_d "  Info1:%d  Info2:%d\n",
		 elem.key, elem.info1, elem.info2);
	  p = (Elem *) Pl_HTBL_Insert(t, (void *) &elem, c - 1);
	  break;

	case 3:
	  p = (Elem *) Pl_HTBL_Find(t, key);
	  break;

	case 4:
	  p = (Elem *) Pl_HTBL_Delete(t, key);
	  break;

	case 5:
	  Pl_HTBL_Delete_All(t);
	  break;

	case 6:
	  k = 0;
	  for (p = (Elem *) Pl_HTBL_First(t, &scan); p;
	       p = (Elem *) Pl_HTBL_Next(&scan))
	    {
	      printf("adr: %#" PL_FMT_x "  (Key:%" PL_FMT_d "  Info1:%d  Info2:%d)\n",
		     (PlLong) p, p->key, p->info1, p->info2);
	      k++;
	    }
	  if (k != Pl_HTBL_Nb_Elements(t))
	    printf("# displayed elements: %d <> %d\n",
		   k, Pl_HTBL_Nb_Elements(t));
	  break;

	case 7:
	  printf("new size: ");
	  scanf("%d", &size);
	  getchar();
	  t = Pl_HTBL_Realloc_Table(t, size);
	  if (t == NULL)
	    printf("Cannot extend the table\n");
	  break;

	case 8:
	  Check_Table(t);
	  break;
	}

      if (c < 5)
	{
	  if (p == NULL)
	    printf("returned value: NULL\n");
	  else
	    printf("returned value: %#" PL_FMT_x "  (Key:%" PL_FMT_d "  Info1:%d  Info2:%d)\n",
		   (PlLong) p, p->key, p->info1, p->info2);
	}

      printf("Nb Elements:%d\n", Pl_HTBL_Nb_Elements(t));
    }

  return 0;
}

#endif /* USE_ALONE */
