/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Tools                                                           *
 * File  : cpt_string.c                                                    *
 * Descr.: compact prefix tree data structure (not used)                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2025 Daniel Diaz                                     *
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
#include <stdbool.h>
#include <locale.h>
#include <assert.h>



#if 1
#define USE_ALONE
#endif

#ifdef USE_ALONE

#define Malloc(size)       malloc(size)
#define Calloc(nb, size)   calloc(nb, size)
#define Realloc(ptr, size) realloc(ptr, size)
#define Free(ptr)          free(ptr)

#else

#include "../EnginePl/engine_pl.h"

#endif


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/* we maintain 2 backward pointers: home_tree and parent_cell 
 * they are not used and could simply be removed
 * if we are happy with recursion (and callback functions).
 * they are needed if we want to extend this implementation
 * with an iterator (usable in a loop as foreach)
 *
 * Here it is ensured that the full word always exist even
 * in a sub-tree. For instance with ab-ricot ab-alone,
 * in the node ricot it is possible to do str-2 to obtain
 * the full word. This could be changed to only malloc
 * portion of texts : user code could avoid the strdup since
 * only needed part would ne allocated here (important gain ?).
 */

typedef struct cptnode *CPTTree;

typedef struct cptcell *CPTList;

typedef struct cptcell
{
  CPTTree child_tree;
  CPTTree owner_tree;		/* pointer to owner node */
  CPTList next;
}
CPTCell;

typedef struct cptnode
{
  char *str;
  int length;
  bool end;
  CPTList list;
  CPTCell *parent_cell;		/* pointer to parent (cell inside list) */
}
CPTNode;

typedef CPTTree CPT;

typedef struct
{
  CPTTree tree;
  char *buff;
  int length;
  bool (*fct)(char *str, int len, int no);
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

static int Common_Prefix_Length(char *str1, int l1, char *str2, int l2);

static CPTTree Create_Node(char *word, int length, bool end, CPTList list, CPTCell *parent_cell);

static CPTList Create_Cell(CPTTree child_tree, CPTTree owner_tree, CPTList next);

CPT CPT_Init(void);

bool CPT_Add_Word(CPT cpt, char *word, int length);

bool CPT_Remove_Word(CPTTree tree, char *word, int length);

bool CPT_Search_Word(CPT tree, char *word, int length);



CPTMatch *CPT_Match_Init(CPT tree, char *prefix, int prefix_length, char *buff,
			 bool (*fct)(char *str, int len, int no));

int CPT_Do_Match(CPTMatch *match);

static int Do_Match_Rec(CPTTree tree, char *buff, int length,
			bool (*fct)(char *str, int len, int no), int no, bool *cont);

char *CPT_Match_Info(CPTMatch *match, int *prefix_length, int *nb_words, int *max_length);

static int Match_Info_Rec(CPTTree tree, int length, int *max_length);

CPTStat *CPT_Statistics(CPT tree);

static void Statistics_Rec(CPT tree, int depth, int prefix_length);


/*
 * In the code, variable/field names 'tree' and 'node' are interchangeable 
 */




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
Create_Node(char *word, int length, bool end, CPTList list, CPTCell *parent_cell)
{
  CPTTree tree = Malloc(sizeof(*tree));

  tree->str = word;
  tree->length = length;
  tree->end = end;
  tree->list = list;

  if (list)	/* only called with list of 0 or 1 element -> a 'if' is enough */
    list->owner_tree = tree;
  tree->parent_cell = parent_cell;

  return tree;
}




/*-------------------------------------------------------------------------*
 * CREATE_CELL                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static CPTList
Create_Cell(CPTTree child_tree, CPTTree owner_tree, CPTList next)
{
  CPTList list = Malloc(sizeof(*list));

  list->child_tree = child_tree;
  list->owner_tree = owner_tree;
  list->next = next;

  return list;
}




/*-------------------------------------------------------------------------*
 * CPT_INIT                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
CPT
CPT_Init(void)
{				/* to ensure initial tree is never NULL */
  return Create_Node(NULL, 0, false, NULL, NULL);
}




/*-------------------------------------------------------------------------*
 * CPT_ADD_WORD                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
bool
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
	    return false;
	  tree->end = true;
	  return true;
	}

      word += l;
      length -= l;

      if (l < tree->length)	/* the stored prefix is not fully consumed */
	{			/* update the stored prefix: */
	  /* create a node  with begin (to replace current) 
	   *    and a child with end   (patching current node) */
	  CPTCell *cell = Create_Cell(tree, NULL, NULL); /* cell->owner_tree set by Create_Node */
	  tree1 = Create_Node(tree->str, l, false, cell, tree->parent_cell);
	  //	  assert(tree1 == tree1->parent_cell->child_tree);
	  tree->str += l;
	  tree->length -= l;
	  tree->parent_cell = cell;
	  //	  assert(tree == tree->parent_cell->child_tree);
	  *ptree = tree = tree1;
	}

      if (length == 0)		/* the word is fully consumed */
	goto word_consumed;

      /* find the appropriate child in the sorted list */
      for (p = &(tree->list); *p && (*p)->child_tree->str[0] < *word; p = &((*p)->next))
	;

      /* no existing child - create a new cell */
      if (*p == NULL || (*p)->child_tree->str[0] > *word)
	{
	  /* create a node for the rest of the word */
	  tree1 = Create_Node(word, length, true, NULL, NULL); /* parent_cell set below */
	  /* insert it in the list */
	  *p = Create_Cell(tree1, tree, *p);
	  tree1->parent_cell = *p;
	  return true;
	}
      /* an existing entry matches */
      ptree = &((*p)->child_tree);
      tree = *ptree;
    }
}




/*-------------------------------------------------------------------------*
 * CPT_REMOVE_WORD                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
bool
CPT_Remove_Word(CPTTree tree, char *word, int length)
{
  CPTList *p0, *p, list;
  int l;

  if (length == 0)		/* do not free first node - i.e. empty word */
    {
      if (!tree->end)
	return false;

      tree->end = false;
      return true;
    }


  p = p0 = NULL;
  for (;;)
    {
      if (length < tree->length || memcmp(word, tree->str, tree->length) != 0)
	return false;

      word += tree->length;
      length -= tree->length;

      if (length == 0)
	{
	  if (!tree->end)
	    return false;

	  if (tree->list)	/* at least 1 child */
	    tree->end = false;
	  else
	    {			/* no child */
	      assert((*p)->child_tree == tree);
	      Free(tree);
	      list = *p;
	      *p = list->next;
	      Free(list);
	      if (p0 == NULL)
		return true;

	      p = p0;		/* tree is the parent tree */
	      tree = (*p)->child_tree;
	    }

	  if (tree->list && tree->list->next == NULL && !tree->end)
	    {			/* only 1 child - collapse */
	      CPTTree child_to_promote = tree->list->child_tree;
	      l = tree->length;
	      (*p)->child_tree = child_to_promote;
	      child_to_promote->parent_cell = *p;
	      Free(tree);
	      tree = child_to_promote;
	      tree->str -= l;
	      tree->length += l;
	    }
	  return true;
	}

      p0 = p;
      for (p = &(tree->list); *p && (*p)->child_tree->str[0] < *word; p = &((*p)->next))
	;

      if (*p == NULL || (*p)->child_tree->str[0] != *word)
	return false;
      /* an existing entry matches */
      tree = (*p)->child_tree;
    }
}




/*-------------------------------------------------------------------------*
 * CPT_SEARCH_WORD                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
bool
CPT_Search_Word(CPT tree, char *word, int length)
{
  CPTList list;

  for (;;)
    {
      if (length < tree->length || memcmp(word, tree->str, tree->length) != 0)
	return false;

      word += tree->length;
      length -= tree->length;

      if (length == 0)
	return tree->end;

      for (list = tree->list; list && list->child_tree->str[0] < *word; list = list->next)
	;

      if (list == NULL || list->child_tree->str[0] != *word)
	return false;

      tree = list->child_tree;
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
  c = rand() % (sizeof(gensym_chars) - 1); /* -1 for the \0 */
  *buff++ = gensym_chars[c];
  prefix_length++;
#endif

  for (;;)
    {
      if (prefix_length < tree->length || memcmp(prefix, tree->str, tree->length) != 0)
	{
	prefix_is_ok:
	  *buff = '\0';
	  return (int) (buff - save_buff);
	}

      if (prefix_length == tree->length)
	{
	  if (!tree->end)
	    goto prefix_is_ok;

	  c = rand() % (sizeof(gensym_chars) - 1);  /* -1 for the \0 */
	  *buff++ = gensym_chars[c];
	  prefix_length++;
	}

      prefix += tree->length;
      prefix_length -= tree->length;

      for (list = tree->list; list && list->child_tree->str[0] < *prefix; list = list->next)
	;

      if (list == NULL || list->child_tree->str[0] != *prefix)
	goto prefix_is_ok;

      tree = list->child_tree;
    }
}




/*-------------------------------------------------------------------------*
 * CPT_INIT_MATCH                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
CPTMatch *
CPT_Match_Init(CPTTree tree, char *prefix, int prefix_length, char *buff,
	       bool (*fct)(char *str, int len, int no))
{
  CPTList list;
  int l;

  match.buff = buff;

  for (;;)
    {
      l = Common_Prefix_Length(prefix, prefix_length, tree->str, tree->length);
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

      for (list = tree->list; list && list->child_tree->str[0] < *prefix; list = list->next)
	;

      if (list == NULL || list->child_tree->str[0] != *prefix)
	return NULL;

      memcpy(buff, tree->str, tree->length);
      buff += tree->length;

      tree = list->child_tree;
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
  bool cont = true;

  if (tree == NULL)		/* should not occur */
    return 0;

  return Do_Match_Rec(tree, match->buff, match->length, match->fct, 0, &cont);
}




/*-------------------------------------------------------------------------*
 * DO_MATCH_REC                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Do_Match_Rec(CPTTree tree, char *buff, int length,
	     bool (*fct)(char *str, int len, int no), int no, bool *cont)
{
  CPTList list;
  char *p = buff + length;

  if (tree->end)
    {
      *cont = (*fct)(buff, length, no);
      no++;
    }

  for (list = tree->list; *cont && list; list = list->next)
    {
      tree = list->child_tree;
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
CPT_Match_Info(CPTMatch *match, int *prefix_length, int *nb_words, int *max_length)
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
      tree = list->child_tree;
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
    Statistics_Rec(list->child_tree, depth, length);

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

#include <ctype.h>

#define BUFF_SIZE  65536	/* largest word */
#define MAX_WORDS  (1024*1024)

char *t_str[MAX_WORDS];
bool t_sorted = true;
size_t nb_str = 0;




void Show_Words(CPT tree);

void Show_Words_Rec(CPT tree, char *prefix, int length);

void Check_Tree(CPT tree);

void Check_Tree_Rec(CPT tree, char *prefix, int length, CPTList check_parent_cell);

void Show_Tree(CPT tree, int level);

bool Display_Completion(char *str, int len, int no);

int Read_String(FILE *f, char *str);



/*-------------------------------------------------------------------------*
 * SHOW_TREE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Show_Tree(CPT tree, int level)
{
  CPTList list;

  printf("%*s<%.*s> %s\n", level, "", tree->length, tree->str, (tree->end) ? "X" : "");
  for (list = tree->list; list; list = list->next)
    Show_Tree(list->child_tree, level + 3);
}




/*-------------------------------------------------------------------------*
 * SHOW_WORDS                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Show_Words(CPT tree)
{
  static char buff[BUFF_SIZE];

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
    Show_Words_Rec(list->child_tree, prefix, length);
}



/*-------------------------------------------------------------------------*
 * CHECK_TREE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_Tree(CPT tree)
{
  static char buff[BUFF_SIZE];

  Check_Tree_Rec(tree, buff, 0, NULL);
}




/*-------------------------------------------------------------------------*
 * CHECK_TREE_REC                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_Tree_Rec(CPT tree, char *prefix, int length, CPTList check_parent_cell)
{
  CPTList list;

  assert(tree->parent_cell == check_parent_cell);
  assert(tree->parent_cell == NULL || tree->parent_cell->child_tree == tree);
  
  memcpy(prefix + length, tree->str, tree->length);
  length += tree->length;
  //  if (tree->end)
  //    printf("%.*s\n", length, prefix);

  /* except at root, if the node is at end of word it must have at least 2 children */
  assert(!tree->parent_cell || tree->end || (tree->list && tree->list->next));

  for (list = tree->list; list; list = list->next)
    {
      assert(list->owner_tree == tree);
      Check_Tree_Rec(list->child_tree, prefix, length, list);
    }
}



/*-------------------------------------------------------------------------*
 * DISPLAY_COMPLETION                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
bool
Display_Completion(char *str, int len, int no)
{
  printf("match %6d  :<%s> len=%d\n", no, str, len);

#if 0
  return no < 5 - 1
#else
  return true;
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
  
  l = (int) strlen(str);
				/* remove trailing '\n' '\r'... */
  while(isspace(str[l - 1]))
    l--;
  str[l] = '\0';
  return l;
}




/*-------------------------------------------------------------------------*
 * CMP_STR                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Cmp_Str(const void *p1, const void *p2)
{
  char *s1 = *(char **) p1;
  char *s2 = *(char **) p2;
  return strcmp(s1, s2);
}




/*-------------------------------------------------------------------------*
 * LOOKUP                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Lookup(char *str)
{
  if (!t_sorted)
    {
      qsort(t_str, nb_str, sizeof(t_str[0]), Cmp_Str);
      t_sorted = true;
    }

  /* printf("SEARCH %ld\n", nb_str); */
  /* for(int i =0;i < nb_str; i++) */
  /*   printf("elem %2d: %s\n", i, t_str[i]); */
  char **p = bsearch(&str, t_str, nb_str, sizeof(t_str[0]), Cmp_Str);
  return (p) ? (int) (p - t_str) : -1;
}




/*-------------------------------------------------------------------------*
 * ADD                                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
bool
Add(CPT cpt, char *str, int l)
{
  str = strdup(str);
  int pos = Lookup(str);
  
  if (!CPT_Add_Word(cpt, str, l))
    {
      assert(pos >= 0);
      free(str);
      return false;
    }

  assert(pos < 0);
  t_str[nb_str++] = str;
  t_sorted = false;
  return true;
}



/*-------------------------------------------------------------------------*
 * READ_FILE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Read_File(CPT cpt, char *file)
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
      if (!Add(cpt, str, l))
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
 * REMOVE                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
bool
Remove(CPT cpt, char *str, int l)
{
  int pos = Lookup(str);

  if (!CPT_Remove_Word(cpt, str, l))
    {
      assert(pos < 0);
      return false;
    }

  assert(pos >= 0);
  t_str[pos] = t_str[--nb_str];
  t_sorted = false;
  return true;
}




/*-------------------------------------------------------------------------*
 * REMOVE_RANDOM                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Remove_Random(CPT cpt, int n)
{
  if (n == 0)
    n = nb_str;
  if (n > nb_str)
    n = nb_str;

  while(n--)
    {
      int pos = rand() % nb_str;
      printf("removing word %s\n", t_str[pos]);
      assert(CPT_Remove_Word(cpt, t_str[pos], strlen(t_str[pos])));
      t_str[pos] = t_str[--nb_str];
    }
  t_sorted = false;
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
  static char str[BUFF_SIZE];
  static char buff[BUFF_SIZE];

  setlocale(LC_ALL, "");

  setbuf(stdout, NULL);		/* for MS VC++ compiler */
  cpt = CPT_Init();

  if (argc > 1)
    Read_File(cpt, argv[1]);

  for (;;)
    {
      Check_Tree(cpt);
      stat = CPT_Statistics(cpt);
      assert(stat->nb_word == nb_str);
      printf("\n----------\n");
      
      enum {END, ADD, READ_FILE, REMOVE, REMOVE_RANDOM, SEARCH, COMPLETION, GENSYM, WORD_LIST, TREE, STATS};

      printf("1-Add  2-Read_File  3-Remove  4-Remove_Random  5-Search  6-Completion  "
	     "7-GenSym  8-Word list  9-Tree  10-Stats  0-end: ");
      scanf("%d", &c);
      getchar();
      if (c == END)
	break;
      switch (c)
	{
	case ADD:
	  printf("word: ");
	  l = Read_String(stdin, str);
	  if (!Add(cpt, str, l))
	    printf("already inserted\n");
	  break;

	case READ_FILE:
	  printf("file: ");
	  l = Read_String(stdin, str);
	  Read_File(cpt, str);
	  break;

	case REMOVE:
	  printf("word: ");
	  l = Read_String(stdin, str);
 	  if (!Remove(cpt, str, l))
	    printf("does not exist\n");
	  break;

	case REMOVE_RANDOM:
	  printf("how much (0: all, N or N%%): ");
	  l = Read_String(stdin, str);
	  char *p;
	  double d = strtod(str, &p);
	  if (*p == '%')
	    d = d * nb_str / 100;
	  Remove_Random(cpt, (int) d);
	  break;

	case SEARCH:
	  printf("word: ");
	  l = Read_String(stdin, str);
	  if (CPT_Search_Word(cpt, str, l))
	    printf("found\n");
	  else
	    printf("not found\n");
	  break;

	case COMPLETION:
	  printf("prefix: ");
	  l = Read_String(stdin, str);
	  if ((match = CPT_Match_Init(cpt, str, l, buff, Display_Completion)))
	    {
	      CPT_Match_Info(match, &l, &n, &m);
	      printf("longest prefix:<%s> length=%d   nb words=%d   longest word=%d\n", buff, l, n, m);
	      printf("Nb displayed:%d\n", CPT_Do_Match(match));
	    }
	  else
	    printf("No matching\n");
	  break;

	case GENSYM:
	  printf("prefix: ");
	  l = Read_String(stdin, str);
	  l = CPT_Gensym(cpt, str, l, buff);
	  printf("new symbol:<%s> length=%d\n", buff, l);
	  if (!Add(cpt, buff, l))
	    printf("ERROR - %s already inserted\n", buff);
	  break;

	case WORD_LIST:
	  Show_Words(cpt);
	  break;

	case TREE:
	  Show_Tree(cpt, 0);
	  break;

	case STATS:
	  stat = CPT_Statistics(cpt);
	  printf("\nGeneral\n");
	  printf("nb words        = %d\n", stat->nb_word);
	  printf("nb nodes        = %d\n", stat->nb_node);
	  printf("nb nodes prefix = %d\n", stat->nb_node2);
	  printf("nb branches     = %d\n", stat->nb_branch);
	  printf("\nBanch sizes\n");
	  printf("longest         = %d\n", stat->max_branch_size);
	  printf("sum sizes       = %d\n", stat->sum_branch_size);
	  printf("avg size        = %g\n", (double) stat->sum_branch_size / stat->nb_branch);
	  printf("sum size / word = %d\n", stat->sum_branch_size_word);
	  printf("avg size / word = %g\n", (double) stat->sum_branch_size_word / stat->nb_word);
	  printf("\nWord lengths\n");
	  printf("longest         = %d\n", stat->max_word_length);
	  printf("longest sub-word= %d\n", stat->max_swrd_length);
	  printf("sum lengths     = %d\n", stat->sum_word_length);
	  printf("avg length      = %g\n", (double) stat->sum_word_length / stat->nb_word);
	  printf("\nList sizes\n");
	  printf("longest         = %d\n", stat->max_list_size);
	  printf("sum sizes       = %d\n", stat->sum_list_size);
	  printf("avg size        = %g\n", (double) stat->sum_list_size / stat->nb_node2);
	  printf("first list size = %d\n", stat->fst_list_size);
	  printf("   excepting first list (ie. first node):\n");
	  printf("longest         = %d\n", stat->max2_list_size);
	  printf("sum' sizes      = %d\n", stat->sum_list_size - stat->fst_list_size);
	  printf("avg' size       = %g\n", (double) (stat->sum_list_size - stat->fst_list_size) / (stat->nb_node2 - 1));
	  break;
	}
    }

  return 0;
}

#endif /* USE_ALONE */
