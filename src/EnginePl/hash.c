/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : hash.c                                                          *
 * Descr.: hash table management                                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2003 Daniel Diaz                                     *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if 0
#define USE_ALONE
#endif

#ifdef USE_ALONE

#include "hash.h"

#define Malloc(size)       malloc(size)
#define Calloc(nb, size)   calloc(nb, size)
#define Realloc(ptr, size) realloc(ptr, size)
#define Free(ptr)          free(ptr)

#else

#include "engine_pl.h"

#endif


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
  long key;
  /* the rest of the elem comes here */
};



/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static HashNode *Hash_Locate(HashNode *t, int tbl_size, long key);



#define HASH_STATIC_SIZE           3

#define Tbl_Size(t)                (((int *)(t))[0])

#define Elem_Size(t)               (((int *)(t))[1])

#define Nb_Elem(t)                 (((int *)(t))[2])

#define Hsh_Table(t)               (((HashNode *) (t)) + HASH_STATIC_SIZE)



#define Hash_Function(k, size)     ((unsigned long) (k) % (size))




/*-------------------------------------------------------------------------*
 * A hash table consists of a header (tbl_size, elem_size, nb_elem) and a  *
 * table of tbl_size pointers to nodes.                                    *
 * Each node records a pointer to the next node, and a user element whose  *
 * size is elem_size. Each element must begin with the key (a long).       *
 *-------------------------------------------------------------------------*/


/*-------------------------------------------------------------------------*
 * HASH_ALLOC_TABLE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Hash_Alloc_Table(int tbl_size, int elem_size)
{
  char *tbl;

  tbl = (char *) Calloc(HASH_STATIC_SIZE + tbl_size, sizeof(HashNode));
#ifdef USE_ALONE
  if (tbl == NULL)
    return NULL;
#endif

  if (tbl_size < 1)
    tbl_size = 1;		/* at least one cell in a table */

  Tbl_Size(tbl) = tbl_size;
  Elem_Size(tbl) = elem_size;
  Nb_Elem(tbl) = 0;

  return tbl;
}




/*-------------------------------------------------------------------------*
 * HASH_REALLOC_TABLE                                                      *
 *                                                                         *
 * NB: user information is not moved elsewhere                             *
 *-------------------------------------------------------------------------*/
char *
Hash_Realloc_Table(char *tbl, int new_tbl_size)
{
  int tbl_size = Tbl_Size(tbl);
  int elem_size = Elem_Size(tbl);
  HashNode *t = Hsh_Table(tbl);
  HashNode *endt = t + tbl_size;
  HashNode p, p1;
  HashNode *prev;
  char *new_tbl;
  HashNode *new_t;

  if ((new_tbl = Hash_Alloc_Table(new_tbl_size, elem_size)) == NULL)
    return NULL;

  Nb_Elem(new_tbl) = Nb_Elem(tbl);

  new_t = Hsh_Table(new_tbl);

  do
    {
      p = *t;
      while (p)
	{			/* here *prev==NULL */
	  prev = Hash_Locate(new_t, new_tbl_size, p->key);
	  p1 = p;
	  p = p->next;
	  *prev = p1;
	  p1->next = NULL;
	}
    }
  while (++t < endt);

  Free(tbl);

  return new_tbl;
}




/*-------------------------------------------------------------------------*
 * HASH_FREE_TABLE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Hash_Free_Table(char *tbl)
{
  int tbl_size = Tbl_Size(tbl);
  HashNode *t = Hsh_Table(tbl);
  HashNode *endt = t + tbl_size;
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
  while (++t < endt);

  Free(tbl);
}




/*-------------------------------------------------------------------------*
 * HASH_DELETE_ALL                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Hash_Delete_All(char *tbl)
{
  int tbl_size = Tbl_Size(tbl);
  HashNode *t = Hsh_Table(tbl);
  HashNode *endt = t + tbl_size;
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
  while (++t < endt);

  Nb_Elem(tbl) = 0;
}




/*-------------------------------------------------------------------------*
 * HASH_UPDATE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Hash_Insert(char *tbl, char *elem, int replace)
{
  int tbl_size = Tbl_Size(tbl);
  int elem_size = Elem_Size(tbl);
  HashNode *t = Hsh_Table(tbl);
  long key = *(long *) elem;
  HashNode *prev;
  HashNode p;

  prev = Hash_Locate(t, tbl_size, key);
  p = *prev;

  if (p == NULL)		/* the key does not exist */
    {
      p =
	(HashNode) Malloc(sizeof(struct hash_node) - sizeof(long) +
			  elem_size);
#ifdef USE_ALONE
      if (p == NULL)
	return NULL;
#endif

      p->next = NULL;
      Nb_Elem(tbl)++;
      *prev = p;
    }
  else if (!replace)
    goto finish;

  memcpy((char *) (&(p->key)), elem, elem_size);

finish:
  return (char *) (&(p->key));
}




/*-------------------------------------------------------------------------*
 * HASH_FIND                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Hash_Find(char *tbl, long key)
{
  int tbl_size = Tbl_Size(tbl);
  HashNode *t = Hsh_Table(tbl);
  HashNode *prev;
  HashNode p;

  prev = Hash_Locate(t, tbl_size, key);
  p = *prev;

  if (p == NULL)		/* the key does not exist */
    return NULL;

  return (char *) (&(p->key));
}




/*-------------------------------------------------------------------------*
 * HASH_DELETE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Hash_Delete(char *tbl, long key)
{
  int tbl_size = Tbl_Size(tbl);
  HashNode *t = Hsh_Table(tbl);
  HashNode *prev;
  HashNode p;

  prev = Hash_Locate(t, tbl_size, key);
  p = *prev;

  if (p == NULL)		/* the key does not exist */
    return NULL;

  *prev = p->next;
  Free(p);
  Nb_Elem(tbl)--;

  return tbl;
}




/*-------------------------------------------------------------------------*
 * HASH_LOCATE                                                             *
 *                                                                         *
 * This function returns the address of the pointer to the node associated *
 * to the key (if the pointer is NULL the key is not in the table).        *
 *-------------------------------------------------------------------------*/
static HashNode *
Hash_Locate(HashNode *t, int tbl_size, long key)
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
 * HASH_FIRST                                                              *
 *                                                                         *
 * Hash_First and Hash_Next make it possible to scan a hash table.         *
 * Example of use:                                                         *
 *                                                                         *
 *   HashScan scan;                                                        *
 *   char     *buff_ptr;                                                   *
 *                                                                         *
 *   for(buff_ptr=Hash_First(tbl,&scan);buff_ptr;buff_ptr=Hash_Next(&scan))*
 *       Display_Element(buff_ptr);                                        *
 *-------------------------------------------------------------------------*/
char *
Hash_First(char *tbl, HashScan *scan)
{
  int tbl_size = Tbl_Size(tbl);
  HashNode *t = Hsh_Table(tbl);
  HashNode *endt = t + tbl_size;


  scan->endt = (char *) endt;
  scan->cur_t = (char *) t;
  scan->cur_p = (char *) (*t);

  return Hash_Next(scan);
}




/*-------------------------------------------------------------------------*
 * HASH_NEXT                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Hash_Next(HashScan *scan)
{
  HashNode *t;
  HashNode *endt;
  HashNode p;

  p = (HashNode) (scan->cur_p);
  if (p)
    {
      scan->cur_p = (char *) (p->next);
      return (char *) (&(p->key));
    }

  t = (HashNode *) (scan->cur_t);
  endt = (HashNode *) (scan->endt);

  while (++t < endt)
    {
      p = *t;
      if (p)
	{
	  scan->cur_t = (char *) t;
	  scan->cur_p = (char *) (p->next);
	  return (char *) (&(p->key));
	}
    }

  return NULL;
}




/*-------------------------------------------------------------------------*
 * HASH_TABLE_SIZE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Hash_Table_Size(char *tbl)
{
  return Tbl_Size(tbl);
}




/*-------------------------------------------------------------------------*
 * HASH_NB_ELEMENTS                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Hash_Nb_Elements(char *tbl)
{
  return Nb_Elem(tbl);
}




#ifdef USE_ALONE

/*-------------------------------------------------------------------------*
 * HASH_CHECK_TABLE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Hash_Check_Table(char *tbl)
{
  int tbl_size = Tbl_Size(tbl);
  HashNode *t = Hsh_Table(tbl);
  HashNode *endt = t + tbl_size;
  HashNode p;
  int i = 0;

  do
    {
      printf("Hash Code:%d\n", t - Hsh_Table(tbl));

      for (p = *t; p; p = p->next, i++)
	printf("\tadr:%#x  key:%ld\n", (int) p, p->key);
    }
  while (++t < endt);

  if (i != Nb_Elem(tbl))
    printf("Nb_Elem counter erroneous = %d insted of %d\n", Nb_Elem(tbl),
	   i);
}


typedef struct
{
  long key;
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
  char *t;
  int size;
  long key;
  Elem elem, *p;
  HashScan scan;
  int c;
  int i = -1;
  int k;

  setbuf(stdout, NULL);
  printf("initial size: ");
  scanf("%d", &size);
  getchar();
  t = Hash_Alloc_Table(size, sizeof(Elem));
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
	  scanf("%ld", &key);
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
	  printf("passed value: Key:%ld  Info1:%d  Info2:%d\n",
		 elem.key, elem.info1, elem.info2);
	  p = (Elem *) Hash_Insert(t, (char *) &elem, c - 1);
	  break;
	  break;

	case 3:
	  p = (Elem *) Hash_Find(t, key);
	  break;

	case 4:
	  p = (Elem *) Hash_Delete(t, key);
	  break;

	case 5:
	  Hash_Delete_All(t);
	  break;

	case 6:
	  k = 0;
	  for (p = (Elem *) Hash_First(t, &scan); p;
	       p = (Elem *) Hash_Next(&scan))
	    {
	      printf("adr: %#lx  (Key:%ld  Info1:%d  Info2:%d)\n",
		     (long) p, p->key, p->info1, p->info2);
	      k++;
	    }
	  if (k != Hash_Nb_Elements(t))
	    printf("# displayed elements: %d <> %d\n",
		   k, Hash_Nb_Elements(t));
	  break;

	case 7:
	  printf("new size: ");
	  scanf("%d", &size);
	  getchar();
	  t = Hash_Realloc_Table(t, size);
	  if (t == NULL)
	    printf("Cannot extend the table\n");
	  break;

	case 8:
	  Hash_Check_Table(t);
	  break;
	}

      if (c < 5)
	{
	  if (p == NULL)
	    printf("returned value: NULL\n");
	  else
	    printf("returned value: %#lx  (Key:%ld  Info1:%d  Info2:%d)\n",
		   (long) p, p->key, p->info1, p->info2);
	}

      printf("Nb Elements:%d\n", Hash_Nb_Elements(t));
    }

  return 0;
}

#endif /* USE_ALONE */
