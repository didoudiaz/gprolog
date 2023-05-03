/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : completion.c                                                    *
 * Descr.: completion facility                                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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

/* This utility provides primitives for completion facility. 
 * This implementation is based on a sorted map provided by map_rbtree.h.
 * (another possibility is to use a radix tree, see Tools/cpt_string.c).
 *
 * This file could be in ../Tools since it does not depend on linedit
 * (it is simply used by linedit) but this needs a bit of work to be done
 * cleanly (if we want to provide several "completers" and not to work on
 * only one global as done here). Doable using map_rbtree.h reexportation
 * with MAP_PROTOTYPES_ONLY.
 *
 * In this version we use opaque pointers in completion.h.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "completion.h"

#define MAP_KEY_TYPE char *
#define MAP_KEY_CMP(x, y) strcmp(x, y)
#define MAP_VALUE_TYPE int
#include "../Tools/map_rbtree.h"


#if 0
#define USE_ALONE
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

static struct map_rbt dico = MAP_INIT;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/


/*-------------------------------------------------------------------------*
 * PL_LE_COMPL_ADD_WORD                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
bool
Pl_LE_Compl_Add_Word(char *word, int word_length)
{
  bool created;
  struct map_entry *entry = map_put(&dico, word, &created);
  if (created)
    entry->value = word_length;
  return created;
}




/*-------------------------------------------------------------------------*
 * PL_LE_COMPL_REMOVE_WORD                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
bool
Pl_LE_Compl_Remove_Word(char *word)
{
  return map_remove(&dico, word);
}




/*-------------------------------------------------------------------------*
 * PL_LE_COMPL_IS_EMPTY                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
bool
Pl_LE_Compl_Is_Empty(void)
{
  return map_is_empty(&dico);
}




/*-------------------------------------------------------------------------*
 * PL_LE_COMPL_SIZE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_LE_Compl_Size(void)
{
  return map_size(&dico);
}




/*-------------------------------------------------------------------------*
 * PL_LE_COMPL_CLEAR                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_LE_Compl_Clear(void)
{
  map_clear(&dico);
}




/*-------------------------------------------------------------------------*
 * PL_LE_COMPL_MATCH_FIRST                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
bool
Pl_LE_Compl_Match_First(ComplMatch *cm, char *prefix, int prefix_length)
{
  /* we don't record the prefix itself because it can change (input buffer in linedit) */
  cm->prefix_length = prefix_length;
  cm->nb_match = 0;
  cm->max_word_length = 0;
  cm->cur_no = 0;
  cm->cur_word = NULL;
  cm->cur_word_length = 0;
  cm->entry = NULL;

  struct map_entry *entry = map_next(&dico, prefix, false);

  if (entry == NULL)
    return false;

  struct map_entry *first_entry = entry;
  struct map_entry *last_entry = NULL;

  /* stop when as soon as a word does not have the wanted prefix 
   * (also check the prefix of the first entry)
   */
  while(entry && strncmp(prefix, entry->key, prefix_length) == 0)
    {
      last_entry = entry;
      cm->nb_match++;
      if (entry->value > cm->max_word_length)
	cm->max_word_length = entry->value;

      entry = map_next_entry(&dico, entry);
    }

  if (cm->nb_match == 0)	/* even the first found did not have the wanted prefix */
    return false;

  cm->cur_word = first_entry->key;
  cm->cur_word_length = first_entry->value;
  cm->entry = (void *) first_entry;

  if (cm->nb_match == 1)
    {
      cm->max_prefix_length = cm->cur_word_length;
    }
  else
    {	  /* since keys are sorted, we can only compare first and last */
      int l = prefix_length;
      while(first_entry->key[l] == last_entry->key[l]) /* test also deals with '\0' */
	l++;
      cm->max_prefix_length = l;
    }
  return true;
}




/*-------------------------------------------------------------------------*
 * PL_LE_COMPL_MATCH_NAVIG                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
bool
Pl_LE_Compl_Match_Navig(ComplMatch *cm, int step)
{
  int no = cm->cur_no + step;
  if (no < 0 || no >= cm->nb_match)
    return false;

  if (step == 0)
    return true;
  
  struct map_entry *entry = (struct map_entry *) cm->entry;
  if (step > 0)
    while(step-- > 0)
      entry = map_next_entry(&dico, entry);
  else
    while(step++ < 0)
      entry = map_prev_entry(&dico, entry);
  
  cm->cur_no = no;
  cm->cur_word = entry->key;
  cm->cur_word_length = entry->value;
  cm->entry = (void *) entry;
  return true;
}




/*-------------------------------------------------------------------------*
 * PL_LE_COMPL_MATCH_FIRST                                                 *
 *                                                                         *
 * Only use prefix_length first characters of prefix (which can be longer  *
 * or can even not be terminated by '\0')                                  *
 *-------------------------------------------------------------------------*/
bool
Pl_LE_Compl_Match_Last(ComplMatch *cm, char *prefix, int prefix_length)
{
  if (!Pl_LE_Compl_Match_First(cm, prefix, prefix_length))
    return false;

  return Pl_LE_Compl_Match_Navig(cm, cm->nb_match - 1);
}




#ifdef USE_ALONE

#include <ctype.h>
#include <locale.h>

#define BUFF_SIZE 1024

/*-------------------------------------------------------------------------*
 * READ_STRING                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Read_String(FILE *f, char *str)
{
  int l;

  if (fgets(str, BUFF_SIZE, f) == NULL)
      return -1;
  
  l = (int) strlen(str);
				/* remove trailing '\n' '\r'... */
  while(isspace(str[l - 1]))
    l--;
  str[l] = '\0';
  return l;
}




/*-------------------------------------------------------------------------*
 * READ_FILE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Read_File(char *file)
{
  FILE * f = fopen(file, "rt");
  if (f == NULL)
    {
      perror("Cannot open file");
      return;
    }

  static char str[BUFF_SIZE];
  int n = 0;
  int d = 0;
  int length = 0;
  int l;

  printf("adding words from file: %s\n", file);
  while ((l = Read_String(f, str)) >= 0)
    {
      if (!Pl_LE_Compl_Add_Word(strdup(str), l))
	{
#if 0
	  printf("Duplicate read: %s\n", str);
#endif
	  d++;
	}
      else
	{
#if 0
	  printf("Added         : %s\n", str);
#endif
	}
      n++;
      length += l;
    }
  fclose(f);
  printf("Read words: %d   added: %d   duplicates: %d   sum length: %d\n", n, n - d, d, length);    
}



/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 * to compile alone active USE_ALONE and simply compile this file.         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  ComplMatch cm;
  int c, l;
  static char str[BUFF_SIZE];

  setlocale(LC_ALL, "");

  setbuf(stdout, NULL);		/* for MS VC++ compiler */

  if (argc > 1)
    Read_File(argv[1]);

  for (;;)
    {
      printf("\n----------\n");
      
      printf("1-Add  2-Read_File  3-Remove  4-Completion  5-list  6-Clear  9-end: ");
      scanf("%d", &c);
      getchar();
      if (c == 9)
	break;
      switch (c)
	{
	case 1:
	  printf("word: ");
	  l = Read_String(stdin, str);
	  if (!Pl_LE_Compl_Add_Word(strdup(str), l))
	    printf("already inserted\n");
	  break;

	case 2:
	  printf("file: ");
	  l = Read_String(stdin, str);
	  Read_File(str);
	  break;

	case 3:
	  printf("word: ");
	  l = Read_String(stdin, str);
 	  if (!Pl_LE_Compl_Remove_Word(str))
	    printf("does not exist\n");
	  break;

	case 4:
	  printf("prefix: ");
	  l = Read_String(stdin, str);
	  bool b = Pl_LE_Compl_Match_First(&cm, str, l);
	  if (b)
	    {
	      printf("longest prefix:<%.*s> length: %d   nb words: %d   longest word length: %d\n",
		     cm.max_prefix_length, cm.cur_word, cm.max_prefix_length, cm.nb_match, cm.max_word_length);
	    }
	  else
	    printf("No matching\n");
	  c = 0;
	  for(; b; b = Pl_LE_Compl_Match_Next(&cm))
	    {
	      printf("%3d: %s\n", cm.cur_no, cm.cur_word);
	      c++;
	    }
	  printf("Nb displayed:%d\n", c);
	  break;

	case 5:
	  c = 0;
	  map_foreach(&dico, entry)
	    {
	      printf("%s\n", entry->key);
	      c++;
	    }
	  printf("nb words: %d\n", c);
	  break;

	case 6:
	  Pl_LE_Compl_Clear();
	  break;
	}
    }

  return 0;
}

#endif /* USE_ALONE */
