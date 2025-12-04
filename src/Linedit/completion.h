/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : completion.h                                                    *
 * Descr.: completion facility - header file                               *
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

#ifndef _COMPLETION_H
#define _COMPLETION_H

#include <stdbool.h>


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/* See explanations in completion.c about opaque pointers */

/*
 * At iteration (Pl_LE_Compl_Match_Navig), the needed (input) fields are
 * nb_match, cur_no (to check displacement) and entry (to naviagte in the map)
 * output fields are: cur_no (updated), cur_word and cur_word_length.
 */
typedef struct
{
  int prefix_length;		/* initial prefix length */
  int nb_match;			/* number of matching words */
  int max_word_length;		/* length of longest matching word */
  int max_prefix_length;	/* length of longest common prefix (use cur_word to obtain it) */
				/* information on current matching word */
  int cur_no;			/* current word index (from 0 to nb_match-1) */
  char *cur_word;		/* current word */
  int cur_word_length;		/* current word length */
  void *entry;			/* opaque pointer (used to iterate on the map) */
}ComplMatch;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/


bool Pl_LE_Compl_Add_Word(char *word, int word_length);

bool Pl_LE_Compl_Remove_Word(char *word);

bool Pl_LE_Compl_Is_Empty(void);

int Pl_LE_Compl_Size(void);

void Pl_LE_Compl_Clear(void);

bool Pl_LE_Compl_Match_First(ComplMatch *cm, char *prefix, int prefix_length);

bool Pl_LE_Compl_Match_Navig(ComplMatch *cm, int step);

#define Pl_LE_Compl_Match_Next(cm) Pl_LE_Compl_Match_Navig((cm), 1)

#define Pl_LE_Compl_Match_Prev(cm) Pl_LE_Compl_Match_Navig((cm), -1)

#define Pl_LE_Compl_Match_Goto(cm, no) Pl_LE_Compl_Match_Navig((cm), (no) - (cm)->cur_no)

#endif
