/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : atom.c                                                          *
 * Descr.: atom table management                                           *
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
#include <locale.h>
#include <ctype.h>

#define ATOM_FILE

#include "engine_pl.h"

#ifndef NO_USE_LINEDIT
#include "linedit.h"
#endif

#if 0
#define DEBUG
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define ERR_ATOM_NIL_INVALID       "atom: invalid ATOM_NIL (should be %d)"

#define ERR_TABLE_FULL_ENV         "Atom table full (max atom: %d, environment variable used: %s)"

#define ERR_TABLE_FULL_NO_ENV      "Atom table full (max atom: %d - fixed size)"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

    /* this variable can be overwritten by top_comp.c (similarl to stacks) */

int pl_char_type[256] = {

/* nul soh stx etx eot enq ack bel bs  ht  nl  vt  np  cr  so  si  */
   LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA,

/* dle dc1 dc2 dc3 dc4 nak syn etb can em sub esc  fs  gs  rs  us  */
   LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA,

/* spc !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /   */
   LA, SC, DQ, GR, GR, CM, GR, QT, PC, PC, GR, GR, SC, GR, GR, GR,

/* 0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?   */
   DI, DI, DI, DI, DI, DI, DI, DI, DI, DI, GR, SC, GR, GR, GR, GR,

/* @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   */
   GR, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL,

/* P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _   */
   CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, PC, GR, PC, GR, UL,

/* `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o    */
   BQ, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL,

/* p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~   del  */
   SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, PC, PC, PC, GR, LA
/*  0x80 ... 0xff = EX (set by Init_Atom)"                           */
};


char pl_escape_symbol[] = "abfnrtv";
char pl_escape_char[] = "\a\b\f\n\r\t\v";


static char str_char[256][2];




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int Add_Atom(char *name, int len, unsigned hash, 
		    AtomInf *patom, Bool allocate);


static AtomInf *Locate_Atom(char *name, unsigned hash);

static unsigned Hash_String(char *str, int len);

static void Error_Table_Full(void);




/*-------------------------------------------------------------------------*
 * PL_INIT_ATOM                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Init_Atom(void)
{
  int i, c;
  
  if (pl_max_atom < 256)
    pl_max_atom = 256;

  if (pl_max_atom <= ATOM_NIL)
    pl_max_atom = ATOM_NIL + 1;	/* to be sure h([]) % pl_max_atom == ATOM_NIL */

  if (pl_max_atom > (1 << ATOM_MAX_BITS)) /* be sure f/n words can be encoded (see wam_inst.h) */
    pl_max_atom = (1 << ATOM_MAX_BITS);

  pl_atom_tbl = (AtomInf *) Calloc(pl_max_atom, sizeof(AtomInf));
  pl_nb_atom = 0;
    

  for (c = 128; c < 256; c++) 
    {
      pl_char_type[c] = islower(c) ? SL : (isupper(c)) ? CL : EX;
    }

  for (i = 0; i < 256; i++)	/* initial conv mapping = identity */
    pl_char_conv[i] = i;


  for (i = 0; i < 256; i++)
    {
      str_char[i][0] = i;
      str_char[i][1] = '\0';
#ifndef OPTIM_1_CHAR_ATOM
      atom_char[i] =
#endif
	Pl_Create_Atom(str_char[i]);
    }

  i = Pl_Create_Atom("[]");
  if (i != ATOM_NIL)
    Pl_Fatal_Error(ERR_ATOM_NIL_INVALID, i);

  pl_atom_void = Pl_Create_Atom("");
  pl_atom_curly_brackets = Pl_Create_Atom("{}");

  pl_atom_false = Pl_Create_Atom("false");
  pl_atom_true = Pl_Create_Atom("true");

  pl_atom_end_of_file = Pl_Create_Atom("end_of_file");
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_ALLOCATE_ATOM                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Create_Allocate_Atom(char *name)
{
  int len = strlen(name);
  unsigned hash = Hash_String(name, len);
  AtomInf *patom = Locate_Atom(name, hash);

  return Add_Atom(name, len, hash, patom, TRUE);
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_ATOM                                                          *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Create_Atom(char *name)
{
  int len = strlen(name);
  unsigned hash = Hash_String(name, len);

  AtomInf *patom = Locate_Atom(name, hash);

  return Add_Atom(name, len, hash, patom, FALSE);
}




/*-------------------------------------------------------------------------*
 * ADD_ATOM                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Add_Atom(char *name, int len, unsigned hash, AtomInf *patom, Bool allocate)
{
  AtomProp prop;
  char *p;
  int c_type;
  Bool identifier;
  Bool graphic;
  patom = Locate_Atom(name, hash);

  if (patom == NULL)
    Error_Table_Full();

  if (patom->name != NULL)
    return patom - pl_atom_tbl;	/* already exists */

  if (allocate)
    name = Strdup(name);

  pl_nb_atom++;

  patom->name = name;
  patom->hash = hash;

  prop.needs_scan = FALSE;

  identifier = graphic = (*name != '\0');

  for (p = name; *p; p++)
    {
      c_type = pl_char_type[(unsigned char) *p];

      if ((c_type & (UL | CL | SL | DI)) == 0)
	identifier = FALSE;

      if (c_type != GR)
	graphic = FALSE;

      if ((*p != ' ' && (c_type & (QT | EX | LA))) || *p == '\\')
	prop.needs_scan = TRUE;
    }

  prop.length = len;

#ifndef NO_USE_LINEDIT
  if (len > 1 && identifier)
    Pl_LE_Compl_Add_Word(name, len);
#endif

  if (pl_char_type[(unsigned char) *name] != SL)	/* small letter */
    identifier = FALSE;


  if (identifier)
    {
      prop.type = IDENTIFIER_ATOM;
      prop.needs_quote = FALSE;
      goto finish;
    }

  if (graphic)
    {
      prop.type = GRAPHIC_ATOM;
      prop.needs_quote =
	(len == 1 && *name == '.') ||
	(len == 1 && *name == '%') ||
	(len >= 2 && name[0] == '/' && name[1] == '*')
#if 0				/* this one does not need quotes it seems */
	|| (len == 2 && name[0] == '*' && name[1] == '/')
#endif
	;
      goto finish;
    }

  if (len == 1 && pl_char_type[(unsigned char) *name] == SC)
    {
      prop.type = SOLO_ATOM;
      prop.needs_quote = (*name == ',');
      goto finish;
    }

  prop.type = OTHER_ATOM;
  prop.needs_quote = prop.needs_scan ||
    !(len == 2 && ((name[0] == '[' && name[1] == ']') ||
		   (name[0] == '{' && name[1] == '}')));


finish:
  prop.op_mask = 0;
  patom->prop = prop;

  return patom - pl_atom_tbl;
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_ATOM_TAGGED                                                   *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Pl_Create_Atom_Tagged(char *name)
{
  return Tag_ATM(Pl_Create_Atom(name));
}




/*-------------------------------------------------------------------------*
 * PL_FIND_ATOM                                                            *
 *                                                                         *
 * return the atom key or -1 if not exist.                                 *
 *-------------------------------------------------------------------------*/
int
Pl_Find_Atom(char *name)
{
  int len = strlen(name);
  unsigned hash = Hash_String(name, len);
  AtomInf *patom;

  patom = Locate_Atom(name, hash);
  return (patom == NULL || patom->name == NULL) ? -1 : patom - pl_atom_tbl;
}




/*-------------------------------------------------------------------------*
 * LOCATE_ATOM                                                             *
 *                                                                         *
 * We use a specific hash table for atoms that cannot be extended but which*
 * provides a unique integer (0..pl_max_atom-1) that could be used in the  *
 * future for tagged ATM words (and for structures).                       *
 *                                                                         *
 * index (in the table) = hash code % pl_max_atom                          *
 *                                                                         *
 * return the address of the found atom (if exists)                        *
 *        the address of the corresponding free cell (if not exist)        *
 *        NULL if the table is full                                        *
 *-------------------------------------------------------------------------*/
static AtomInf *
Locate_Atom(char *name, unsigned hash)
{
  int index;
  AtomInf *patom0, *patom, *endt;

  index = hash % pl_max_atom;
  patom = patom0 = pl_atom_tbl + index;
  endt = pl_atom_tbl + pl_max_atom;

  while (patom->name && (patom->hash != hash || strcmp(patom->name, name) != 0))
    {
      patom++;
      if (patom == endt)
	patom = pl_atom_tbl;
      if (patom == patom0)	/* one complete round: the table is full */
	return NULL;
    }

#if 0
  if (patom->name == NULL)
    {
      if (patom != patom0)
	{
	  printf("atom: (%s) collision ixd: %ld -> %ld\n", name, patom0 - pl_atom_tbl, patom - pl_atom_tbl);
	}

      if (hash !=  patom - pl_atom_tbl)
	{
	  printf("atom: (%s)   hash: %u   idx: %ld\n", name, hash, patom - pl_atom_tbl);
	}
    }
#endif

#if 0
  if (patom->name == NULL)
    {
      if (hash != index)
	{
	  printf("atom: (%s)   hash: %u   initial idx: %d\n", name, hash, index);
	}
    }
#endif

  return patom;
}





/*-------------------------------------------------------------------------*
 * HASH_STRING                                                             *
 *                                                                         *
 * This function computes a hash key from a string.                        *
 *-------------------------------------------------------------------------*/
static unsigned
Hash_String(char *str, int len)
{
#ifdef OPTIM_1_CHAR_ATOM
  if (len == 1)			/* for 1 char strings: key = char */
    return (unsigned) ((unsigned char) (*str));
#endif

#if 1 /* uncomment to force a given ATOM_NIL (e.g. 256 ?) */
  if (len == 2 && str[0] == '[' && str[1] == ']')
    return ATOM_NIL;
#endif

#if 0
  if (len == 2 && str[0] == '[' && str[1] == ']')
    printf("Hash([]) = %d\n", Pl_Hash_Buffer(str, len));
#endif

  return Pl_Hash_Buffer(str, len);
}




/*-------------------------------------------------------------------------*
 * PL_GEN_NEW_ATOM                                                         *
 *                                                                         *
 * Find a new atom (gensym) beginning by a given prefix.                   *
 *-------------------------------------------------------------------------*/
int
Pl_Gen_New_Atom(char *prefix)
{
#define GEN_SYM_CHARS "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

  static char gen_sym_chars[] = GEN_SYM_CHARS;
  static char gen_sym_buff[1024];

#define Gen_Sym_Rand()  (gen_sym_rand_next = gen_sym_rand_next * 1103515245 + 12345, (gen_sym_rand_next / 65536 % 32768))

  static unsigned gen_sym_rand_next = 1; /* a simple RNG independent from the main one */

#define TRY_MAX 1		/* 1 seems better than anything else on average ! */

#ifdef DEBUG
  static int nb = 0;
  static unsigned long sum_len = 0;
  static unsigned max_len = 0;
  static unsigned long sum_try = 0;
  static unsigned max_try = 0;
  int try_count = 0;
  static double time0 = 0;
  double time, tsec = 0.0;
#endif

  int try_no = 0;
  int len;
  unsigned hash;
  char *str;
  int c;
  AtomInf *patom;
  int atom;

  if (pl_nb_atom >= pl_max_atom)
    Error_Table_Full();


#ifdef DEBUG
  nb++;
  /* printf("GEN_SYM PREFIX : %s\n", prefix); */
#endif

  strcpy(gen_sym_buff, prefix);
  str = gen_sym_buff + strlen(prefix);

  for(;;)
    {
      c = Gen_Sym_Rand() % (sizeof(gen_sym_chars) - 1); /* NB: -1 for '\0' */
      *str = gen_sym_chars[c];
      str[1] = '\0';

      len =  str - gen_sym_buff + 1;

      hash = Hash_String(gen_sym_buff, len);

#if 1
      patom = Locate_Atom(gen_sym_buff, hash);
#else
      patom = pl_atom_tbl + (hash % pl_max_atom);
#endif

#ifdef DEBUG
      try_count++;
      /*      printf("GEN_SYM TRY %3d: %s   len: %d\n", try_count, gen_sym_buff, len); */
#endif

      if (patom->name == NULL)
	break;

      if (++try_no == TRY_MAX)
	{
#if 0
	  c = Gen_Sym_Rand() % (sizeof(gen_sym_chars) - 1); /* NB: -1 for '\0' */
	  *str++ = gen_sym_chars[c];
#else
	  str++;
#endif
	  try_no = 0;
	}
    }


  atom = Add_Atom(gen_sym_buff, len, hash, patom, TRUE);

#ifdef DEBUG
  sum_try += try_count;
  if (try_count > max_try)
    max_try = try_count;
  c = len - strlen(prefix);
  sum_len += c;
  if (c > max_len)
    max_len = c;
  if (nb % 1000 == 0)
    {
      time = (double) Pl_M_User_Time(); /* time needed for the last 1000 gensym */
      tsec = (time - time0) / 1000.0;
      time0 = time;
      printf("GENSYM #%5d: %s len:%d  len add:%d  (avg:%d  max:%d)  try:%d (avg:%d max:%d) time:%.3f\n", 
	     nb, gen_sym_buff, (int) strlen(gen_sym_buff), c, 
	     (int) (sum_len / nb), max_len,
	     try_count, (int) (sum_try / nb), max_try,
	     tsec);
    }
#endif


  return atom;
}




/*-------------------------------------------------------------------------*
 * PL_FIND_NEXT_ATOM                                                       *
 *                                                                         *
 * returns the atom next after 'last_atom' (-1 to start) or -1 at the end  *
 *-------------------------------------------------------------------------*/
int
Pl_Find_Next_Atom(int last_atom)
{
  while (++last_atom < pl_max_atom)
    {
      if (pl_atom_tbl[last_atom].name)
	return last_atom;
    }

  return -1;
}



/*-------------------------------------------------------------------------*
 * ERROR_TABLE_FULL                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Error_Table_Full(void)
{
  if (pl_fixed_sizes)
    Pl_Fatal_Error(ERR_TABLE_FULL_NO_ENV, pl_max_atom);
  else
    Pl_Fatal_Error(ERR_TABLE_FULL_ENV, pl_max_atom, ENV_VAR_MAX_ATOM);
}
