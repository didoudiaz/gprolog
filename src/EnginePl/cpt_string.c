/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Common tool                                                     *
 * File  : cpt_string.c                                                    *
 * Descr.: (longest) common prefix tree management                         *
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#if 1
#define USE_ALONE
#endif

#ifdef USE_ALONE

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

typedef struct cptnode *CPTTree;

typedef struct cptcell *CPTList;

typedef struct cptcell
{
  CPTTree tree;
  CPTList next;
}
CPTCell;

typedef struct cptnode
{
  char *str;
  int length;
  int end;
  CPTList list;
}
CPTNode;

typedef CPTTree CPT;

typedef struct
{
  CPTTree tree;
  char *buff;
  int length;
  int (*fct) ();
}
CPTMatch;

typedef struct
{
  int nb_word;
  int nb_node;
  int nb_node2;
  int nb_branch;
  int max_branch_size;
  int sum_branch_size_word;
  int sum_branch_size;
  int max_word_length;
  int sum_word_length;
  int max_swrd_length;
  int fst_list_size;
  int max_list_size;
  int max2_list_size;
  int sum_list_size;
}
CPTStat;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static CPTMatch match;

static CPTStat stat;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

CPT CPT_Init(void);

int CPT_Add_Word(CPT cpt, char *word, int length);

static int Common_Prefix_Length(char *str1, int l1, char *str2, int l2);

static CPTTree Create_Node(char *word, int length, int end, CPTList list);

static CPTList Create_Cell(CPTTree tree, CPTList next);

int CPT_Search_Word(CPT tree, char *word, int length);



CPTMatch *CPT_Init_Match(CPT tree, char *prefix,
			 int prefix_length, char *buff, int (*fct) ());

int CPT_Do_Match(CPTMatch *match);

static int Do_Match_Rec(CPTTree tree, char *buff, int length,
			int (*fct) (), int no, int *cont);

char *CPT_Match_Info(CPTMatch *match, int *prefix_length,
		     int *nb_words, int *max_length);

static int Match_Info_Rec(CPTTree tree, int length, int *max_length);

CPTStat *CPT_Statistics(CPT tree);

static void Statistics_Rec(CPT tree, int depth, int prefix_length);




/*-------------------------------------------------------------------------*
 * CPT_INIT                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
CPT
CPT_Init(void)
{				/* to ensure initial tree is never NULL */
  return Create_Node(NULL, 0, 0, NULL);
}




/*-------------------------------------------------------------------------*
 * CPT_ADD_WORD                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
CPT_Add_Word(CPT tree, char *word, int length)
{
  int l;
  CPTTree *ptree, tree1;
  CPTList *p;

  for (;;)
    {
      l = Common_Prefix_Length(word, length, tree->str, tree->length);

      if (l == length && l == tree->length)
	{			/* both the word and the prefix are fully consumed */
	word_consumed:
	  if (tree->end)
	    return 1;
	  tree->end = 1;
	  return 0;
	}

      word += l;
      length -= l;

      if (l < tree->length)	/* the stored prefix is not fully consumed */
	{			/* update the stored prefix: */
	  /* create a node with begin and a son with end */
	  tree1 = Create_Node(tree->str, l, 0, Create_Cell(tree, NULL));
	  tree->str += l;
	  tree->length -= l;
	  *ptree = tree = tree1;
	}

      if (length == 0)		/* the word is fully consumed */
	goto word_consumed;

      /* create a node for the rest of the word */
      tree1 = Create_Node(word, length, 1, NULL);

      /* find the appropriate son in the sorted list */
      for (p = &(tree->list); *p && (*p)->tree->str[0] < *word;
	   p = &((*p)->next))
	;

      /* no existing son - create a new cell */
      if (*p == NULL || (*p)->tree->str[0] > *word)
	{
	  *p = Create_Cell(tree1, *p);
	  return 0;
	}
      /* an existing entry matches */
      ptree = &((*p)->tree);
      tree = *ptree;
    }
}




/*-------------------------------------------------------------------------*
 * COMMON_PREFIX_LENGTH                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Common_Prefix_Length(char *str1, int l1, char *str2, int l2)
{
  if (l1 < l2)			/* l2 is the min of l1 and l2 */
    l2 = l1;

  l1 = 0;

  while (l1 < l2 && *str1 == *str2)
    {
      str1++;
      str2++;
      l1++;
    }

  return l1;
}




/*-------------------------------------------------------------------------*
 * CREATE_NODE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static CPTTree
Create_Node(char *word, int length, int end, CPTList list)
{
  CPTTree tree = (CPTTree) Malloc(sizeof(CPTNode));

  tree->str = word;
  tree->length = length;
  tree->end = end;
  tree->list = list;

  return tree;
}




/*-------------------------------------------------------------------------*
 * CREATE_CELL                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static CPTList
Create_Cell(CPTTree tree, CPTList next)
{
  CPTList list = (CPTList) Malloc(sizeof(CPTCell));

  list->tree = tree;
  list->next = next;

  return list;
}




/*-------------------------------------------------------------------------*
 * CPT_REMOVE_WORD                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
CPT_Remove_Word(CPTTree tree, char *word, int length)
{
  CPTList *p0, *p, list;
  int l;

  if (length == 0)		/* do not free first node - i.e. empty word */
    {
      if (!tree->end)
	return 0;

      tree->end = 0;
      return 1;
    }


  p = p0 = NULL;
  for (;;)
    {
      if (length < tree->length
	  || memcmp(word, tree->str, tree->length) != 0)
	return 0;

      word += tree->length;
      length -= tree->length;

      if (length == 0)
	{
	  if (!tree->end)
	    return 0;

	  if (tree->list)	/* at least 1 son */
	    tree->end = 0;
	  else
	    {			/* no son */
	      Free(tree);
	      list = *p;
	      *p = list->next;
	      Free(list);
	      if (p0 == NULL)
		return 1;

	      p = p0;		/* tree is the parent tree */
	      tree = (*p)->tree;
	    }

	  if (tree->list && tree->list->next == NULL && !tree->end)
	    {			/* only 1 son - collapse */
	      l = tree->length;
	      (*p)->tree = tree->list->tree;
	      Free(tree);
	      tree = (*p)->tree;
	      tree->str -= l;
	      tree->length += l;
	    }
	  return 1;
	}

      p0 = p;
      for (p = &(tree->list); *p && (*p)->tree->str[0] < *word;
	   p = &((*p)->next))
	;

      if (*p == NULL || (*p)->tree->str[0] != *word)
	return 0;
      /* an existing entry matches */
      tree = (*p)->tree;
    }
}




/*-------------------------------------------------------------------------*
 * CPT_SEARCH_WORD                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
CPT_Search_Word(CPT tree, char *word, int length)
{
  CPTList list;

  for (;;)
    {
      if (length < tree->length
	  || memcmp(word, tree->str, tree->length) != 0)
	return 0;

      word += tree->length;
      length -= tree->length;

      if (length == 0)
	return tree->end;

      for (list = tree->list; list && list->tree->str[0] < *word;
	   list = list->next)
	;

      if (list == NULL || list->tree->str[0] != *word)
	return 0;

      tree = list->tree;
    }
}




/*-------------------------------------------------------------------------*
 * CPT_GENSYM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
CPT_Gensym(CPTTree tree, char *prefix, int prefix_length, char *buff)
#if 1
#define GENSYM_CHARS "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
#else
#define GENSYM_CHARS "abc"
#endif
{
  CPTList list;
  int c;
  char *save_buff;
  static char gensym_chars[] = GENSYM_CHARS;

  if (buff != prefix)
    {
      memcpy(buff, prefix, prefix_length);
      prefix = buff;
    }
  save_buff = buff;
  buff += prefix_length;

#if 1
  c = rand() % (sizeof(gensym_chars) - 1);
  *buff++ = gensym_chars[c];
  prefix_length++;
#endif

  for (;;)
    {
      if (prefix_length < tree->length ||
	  memcmp(prefix, tree->str, tree->length) != 0)
	{
	prefix_is_ok:
	  *buff = '\0';
	  return buff - save_buff;
	}

      if (prefix_length == tree->length)
	{
	  if (!tree->end)
	    goto prefix_is_ok;

	  c = rand() % (sizeof(gensym_chars) - 1);
	  *buff++ = gensym_chars[c];
	  prefix_length++;
	}

      prefix += tree->length;
      prefix_length -= tree->length;

      for (list = tree->list; list && list->tree->str[0] < *prefix;
	   list = list->next)
	;

      if (list == NULL || list->tree->str[0] != *prefix)
	goto prefix_is_ok;

      tree = list->tree;
    }
}




/*-------------------------------------------------------------------------*
 * CPT_INIT_MATCH                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
CPTMatch *
CPT_Init_Match(CPTTree tree, char *prefix, int prefix_length,
	       char *buff, int (*fct) ())
{
  CPTList list;
  int l;

  match.buff = buff;

  for (;;)
    {
      l =
	Common_Prefix_Length(prefix, prefix_length, tree->str,
			     tree->length);
      if (l == prefix_length)
	{
	  match.tree = tree;
	  memcpy(buff, tree->str, tree->length);
	  buff += tree->length;
	  *buff = '\0';
	  match.length = buff - match.buff;
	  match.fct = fct;
	  return &match;
	}

      if (l < tree->length)
	return NULL;

      prefix += l;
      prefix_length -= l;

      for (list = tree->list; list && list->tree->str[0] < *prefix;
	   list = list->next)
	;

      if (list == NULL || list->tree->str[0] != *prefix)
	return NULL;

      memcpy(buff, tree->str, tree->length);
      buff += tree->length;

      tree = list->tree;
    }
}




/*-------------------------------------------------------------------------*
 * CPT_DO_MATCH                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
CPT_Do_Match(CPTMatch *match)
{
  CPTTree tree = match->tree;
  int cont = 1;

  if (tree == NULL)		/* should not occur */
    return 0;

  return Do_Match_Rec(tree, match->buff, match->length, match->fct, 0,
		      &cont);
}




/*-------------------------------------------------------------------------*
 * DO_MATCH_REC                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Do_Match_Rec(CPTTree tree, char *buff, int length, int (*fct) (),
	     int no, int *cont)
{
  CPTList list;
  char *p = buff + length;

  if (tree->end)
    {
      *cont = (*fct) (buff, length, no);
      no++;
    }

  for (list = tree->list; *cont && list; list = list->next)
    {
      tree = list->tree;
      memcpy(p, tree->str, tree->length);
      p[tree->length] = '\0';

      no = Do_Match_Rec(tree, buff, length + tree->length, fct, no, cont);
    }

  return no;
}




/*-------------------------------------------------------------------------*
 * CPT_MATCH_INFO                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
CPT_Match_Info(CPTMatch *match,
	       int *prefix_length, int *nb_words, int *max_length)
{
  CPTTree tree = match->tree;

  if (tree == NULL)
    return NULL;

  *prefix_length = match->length;

  *max_length = 0;
  *nb_words = Match_Info_Rec(tree, match->length, max_length);

  return match->buff;
}




/*-------------------------------------------------------------------------*
 * MATCH_INFO_REC                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Match_Info_Rec(CPTTree tree, int length, int *max_length)
{
  int nb_word = 0;
  CPTList list;

  if (tree->end)
    {
      nb_word = 1;
      if (length > *max_length)
	*max_length = length;
    }
  else
    nb_word = 0;

  for (list = tree->list; list; list = list->next)
    {
      tree = list->tree;
      nb_word += Match_Info_Rec(tree, length + tree->length, max_length);
    }

  return nb_word;
}




/*-------------------------------------------------------------------------*
 * CPT_STATISTICS                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
CPTStat *
CPT_Statistics(CPT tree)
{
  memset((void *) &stat, 0, sizeof(CPTStat));
  Statistics_Rec(tree, 0, 0);
  return &stat;
}




/*-------------------------------------------------------------------------*
 * STATISTICS_REC                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Statistics_Rec(CPT tree, int depth, int length)
{
  int n;
  CPTList list;

  stat.nb_node++;
  depth++;
  if (depth > stat.max_branch_size)
    stat.max_branch_size = depth;

  length += tree->length;

  if (tree->list && tree->length > stat.max_swrd_length)
    stat.max_swrd_length = tree->length;

  if (tree->end)
    {
      stat.sum_branch_size_word += depth;
      stat.nb_word++;
      stat.sum_word_length += length;
      if (length > stat.max_word_length)
	stat.max_word_length = length;
    }

  for (n = 0, list = tree->list; list; list = list->next, n++)
    Statistics_Rec(list->tree, depth, length);

  if (n == 0)
    {
      stat.nb_branch++;
      stat.sum_branch_size += depth;
    }
  else
    stat.nb_node2++;

  if (depth == 1)
    stat.fst_list_size = n;

  stat.sum_list_size += n;
  if (n > stat.max_list_size)
    stat.max_list_size = n;
  if (depth > 1 && n > stat.max2_list_size)
    stat.max2_list_size = n;
}



#ifdef USE_ALONE

#define BUFF_SIZE  1024

void Show_Words(CPT tree);

void Show_Words_Rec(CPT tree, char *prefix, int length);

void Show_Tree(CPT tree, int level);

int Display_Completion(char *str, int length, int no);

int Read_String(FILE *f, char *str);




/*-------------------------------------------------------------------------*
 * SHOW_TREE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Show_Tree(CPT tree, int level)
{
  CPTList list;

  printf("%*s<%.*s> %s\n", level, "", tree->length, tree->str,
	 (tree->end) ? "X" : "");
  for (list = tree->list; list; list = list->next)
    Show_Tree(list->tree, level + 3);
}




/*-------------------------------------------------------------------------*
 * SHOW_WORDS                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Show_Words(CPT tree)
{
  char buff[BUFF_SIZE];

  Show_Words_Rec(tree, buff, 0);
}




/*-------------------------------------------------------------------------*
 * SHOW_WORDS_REC                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Show_Words_Rec(CPT tree, char *prefix, int length)
{
  CPTList list;

  memcpy(prefix + length, tree->str, tree->length);
  length += tree->length;
  if (tree->end)
    printf("%.*s\n", length, prefix);

  for (list = tree->list; list; list = list->next)
    Show_Words_Rec(list->tree, prefix, length);
}



/*-------------------------------------------------------------------------*
 * DISPLAY_COMPLETION                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Display_Completion(char *str, int length, int no)
{
  printf("match %6d  :<%s> len=%d\n", no, str, length);

#if 0
  return no < 5 - 1
#else
  return 1;
#endif
}




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
  l = strlen(str) - 1;		/* remove trailing '\n' */
  str[l] = '\0';
  return l;
}




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 * to compile alone active USE_ALONE and simply compile this file.         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  CPT cpt;
  CPTMatch *match;
  CPTStat *stat;
  int c, l, n, m;
  char str[BUFF_SIZE];
  char buff[BUFF_SIZE];
  FILE *f;

  setbuf(stdout, NULL);		/* for MS VC++ compiler */
  cpt = CPT_Init();

  if (argc > 1 && (f = fopen(argv[1], "rt")) != NULL)
    {
      int n = 0;
      int d = 0;
      int length = 0;

      printf("adding words from file: %s\n", argv[1]);
      while ((l = Read_String(f, str)) >= 0)
	{
	  d += CPT_Add_Word(cpt, strdup(str), l);
	  n++;
	  length += l;
	}
      fclose(f);
      printf("Added %d words-%d duplicates=%d - sum length=%d\n",
	     n, d, n - d, length);
    }


  for (;;)
    {
      printf("1-Add     2-Remove  3-Search   4-Completion   5-GenSym\n");
      printf("6-Words   7-Tree    8-Stats    9-end: ");
      scanf("%d", &c);
      getchar();
      if (c == 9)
	break;

      switch (c)
	{
	case 1:
	  printf("word: ");
	  l = Read_String(stdin, str);
	  if (CPT_Add_Word(cpt, strdup(str), l))
	    printf("already inserted\n");
	  break;

	case 2:
	  printf("word: ");
	  l = Read_String(stdin, str);
	  if (!CPT_Remove_Word(cpt, str, l))
	    printf("does not exist\n");
	  break;

	case 3:
	  printf("word: ");
	  l = Read_String(stdin, str);
	  if (CPT_Search_Word(cpt, str, l))
	    printf("found\n");
	  else
	    printf("not found\n");
	  break;

	case 4:
	  printf("prefix: ");
	  l = Read_String(stdin, str);
	  if (
	      (match =
	       CPT_Init_Match(cpt, str, l, buff, Display_Completion)))
	    {
	      CPT_Match_Info(match, &l, &n, &m);
	      printf("longest prefix:<%s> length=%d"
		     "   nb words=%d   longest word=%d\n", buff, l, n, m);
	      printf("Nb displayed:%d\n", CPT_Do_Match(match));
	    }
	  else
	    printf("No matching\n");
	  break;

	case 5:
	  printf("prefix: ");
	  l = Read_String(stdin, str);
	  l = CPT_Gensym(cpt, str, l, buff);
	  printf("new symbol:<%s> length=%d\n", buff, l);
	  if (CPT_Add_Word(cpt, strdup(buff), l))
	    printf("ERROR - %s already inserted\n", buff);
	  break;

	case 6:
	  Show_Words(cpt);
	  break;

	case 7:
	  Show_Tree(cpt, 0);
	  break;

	case 8:
	  stat = CPT_Statistics(cpt);
	  printf("\nGeneral\n");
	  printf("nb words        = %d\n", stat->nb_word);
	  printf("nb nodes        = %d\n", stat->nb_node);
	  printf("nb nodes prefix = %d\n", stat->nb_node2);
	  printf("nb branches     = %d\n", stat->nb_branch);
	  printf("\nBanch sizes\n");
	  printf("longest         = %d\n", stat->max_branch_size);
	  printf("sum sizes       = %d\n", stat->sum_branch_size);
	  printf("avg size        = %g\n", (double) stat->sum_branch_size /
		 stat->nb_branch);
	  printf("sum size / word = %d\n", stat->sum_branch_size_word);
	  printf("avg size / word = %g\n", (double)
		 stat->sum_branch_size_word / stat->nb_word);
	  printf("\nWord lengths\n");
	  printf("longest         = %d\n", stat->max_word_length);
	  printf("longest sub-word= %d\n", stat->max_swrd_length);
	  printf("sum lengths     = %d\n", stat->sum_word_length);
	  printf("avg length      = %g\n", (double) stat->sum_word_length /
		 stat->nb_word);
	  printf("\nList sizes\n");
	  printf("longest         = %d\n", stat->max_list_size);
	  printf("sum sizes       = %d\n", stat->sum_list_size);
	  printf("avg size        = %g\n", (double) stat->sum_list_size /
		 stat->nb_node2);
	  printf("first list size = %d\n", stat->fst_list_size);

	  printf("   excepting first list (ie. first node):\n");
	  printf("longest         = %d\n", stat->max2_list_size);
	  printf("sum' sizes      = %d\n", stat->sum_list_size -
		 stat->fst_list_size);
	  printf("avg' size       = %g\n", (double) (stat->sum_list_size -
						     stat->fst_list_size) /
		 (stat->nb_node2 - 1));
	  break;


	}
    }

  return 0;
}

#endif /* USE_ALONE */
