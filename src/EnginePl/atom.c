/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : atom.c                                                          *
 * Descr.: atom table management                                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2012 Daniel Diaz                                     *
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

/* $Id$ */

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

#define ERR_ATOM_TBL_FULL          "Atom table full"
#define ERR_ATOM_NIL_INVALID       "atom: invalid ATOM_NIL (should be %d)"




#define RADIX                      67
#define INV_RADIX_MOD_MAX_ATOM     281707	/* see prog. euclide.c */




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

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

static int hash_weight_tbl[256];
static int hash_inv_tbl[RADIX];

static char gen_sym_buff[1024];




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static AtomInf *Locate_Atom(char *name);

static int Hash_String(char *str);

static char *Gen_Sym(char *prefix, int gen_sym_hash);




/*-------------------------------------------------------------------------*
 * PL_INIT_ATOM                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Init_Atom(void)
{
  int i, c;

  for (i = 0; i < 256; i++)
    hash_weight_tbl[i] = i % RADIX;

  for (i = 0; i < 10; i++)
    {
      hash_weight_tbl[i + '0'] = i;
      hash_inv_tbl[i] = i + '0';
    }

  for (i = 0; i < 26; i++)
    {
      hash_weight_tbl[i + 'A'] = i + 10;
      hash_inv_tbl[i + 10] = i + 'A';
    }


  for (i = 0; i < 26; i++)
    {
      hash_weight_tbl[i + 'a'] = i + 10 + 26;
      hash_inv_tbl[i + 10 + 26] = i + 'a';
    }

  i = 10 + 26 + 26;
  hash_weight_tbl['#'] = i;
  hash_inv_tbl[i] = '#';

  i++;
  hash_weight_tbl['$'] = i;
  hash_inv_tbl[i] = '$';

  i++;
  hash_weight_tbl['&'] = i;
  hash_inv_tbl[i] = '&';

  i++;
  hash_weight_tbl['_'] = i;
  hash_inv_tbl[i] = '_';

  i++;
  hash_weight_tbl['@'] = i;
  hash_inv_tbl[i] = '@';

  for (c = 128; c < 256; c++) 
    {
      pl_char_type[c] = islower(c) ? SL : (isupper(c)) ? CL : EX;
    }

  for (i = 0; i < 256; i++)	/* initial conv mapping = identity */
    pl_char_conv[i] = i;



  pl_nb_atom = 0;

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
  AtomInf *patom;
  char *name1;

  patom = Locate_Atom(name);

  if (patom == NULL)
    Pl_Fatal_Error(ERR_ATOM_TBL_FULL);

  if (patom->name != NULL)
    return patom - pl_atom_tbl;	/* already exists */

  name1 = Strdup(name);

  return Pl_Create_Atom(name1);
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_ATOM                                                          *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Create_Atom(char *name)
{
  AtomInf *patom;
  AtomProp prop;
  char *p;
  int c_type;
  int lg;
  Bool identifier;
  Bool graphic;

  patom = Locate_Atom(name);

  if (patom == NULL)
    Pl_Fatal_Error(ERR_ATOM_TBL_FULL);

  if (patom->name != NULL)
    return patom - pl_atom_tbl;	/* already exists */

  pl_nb_atom++;

  patom->name = name;
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

  prop.length = lg = p - name;

#ifndef NO_USE_LINEDIT
  if (lg > 1 && identifier)
    Pl_LE_Compl_Add_Word(name, lg);
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
	(lg == 1 && *name == '.') ||
	(lg == 1 && *name == '%') ||
	(lg >= 2 && name[0] == '/' && name[1] == '*')
#if 0				/* this one does not need quotes it seems */
	|| (lg == 2 && name[0] == '*' && name[1] == '/')
#endif
	;
      goto finish;
    }

  if (lg == 1 && pl_char_type[(unsigned char) *name] == SC)
    {
      prop.type = SOLO_ATOM;
      prop.needs_quote = (*name == ',');
      goto finish;
    }

  prop.type = OTHER_ATOM;
  prop.needs_quote = prop.needs_scan ||
    !(lg == 2 && ((name[0] == '[' && name[1] == ']') ||
		  (name[0] == '{' && name[1] == '}')));


finish:
  prop.op_mask = 0;
  patom->prop = prop;

  return patom - pl_atom_tbl;
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_ATOM                                                          *
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
  AtomInf *patom;

  patom = Locate_Atom(name);
  return (patom == NULL || patom->name == NULL) ? -1 : patom - pl_atom_tbl;
}




/*-------------------------------------------------------------------------*
 * LOCATE_ATOM                                                             *
 *                                                                         *
 * We use a specific hash table for atoms that cannot be extended but which*
 * provides a unique integer (in 0..MAX_ATOM-1) that could be used in the  *
 * future for tagged ATM words (and for structures).                       *
 *                                                                         *
 * return the address of the found atom (if exists)                        *
 *        the address of the corresponding free cell (if not exist)        *
 *        NULL if the table is full                                        *
 *-------------------------------------------------------------------------*/
static AtomInf *
Locate_Atom(char *name)
{
  int n;
  AtomInf *patom, *endt;

  if (pl_nb_atom == MAX_ATOM)
    return NULL;

  n = Hash_String(name);
  /* here either the atom is in the table */
  /* or there is at least one free cell.  */
  patom = pl_atom_tbl + n;
  endt = pl_atom_tbl + MAX_ATOM;

  while (patom->name && strcmp(patom->name, name) != 0)
    {
      patom++;
      if (patom == endt)
	patom = pl_atom_tbl;
    }

  return patom;
}





/*-------------------------------------------------------------------------*
 * HASH_STRING                                                             *
 *                                                                         *
 * This function computes a hash key from a string. If you modify MAX_ATOM *
 * or RADIX modify INV_RADIX_MOD_MAX_ATOM (see prog. euclide.c). MAX_ATOM  *
 * must be > 255. RADIX must be a prime number <256, the current value (67)*
 * does not need to be modified (or else update hash_weight_tbl and        *
 * hash_inv_tbl to control characters produced by Gen_Sym()).              *
 *-------------------------------------------------------------------------*/
static int
Hash_String(char *str)
{
  int l = strlen(str);
  char *p = str + l;
  unsigned n = 0;

#ifdef OPTIM_1_CHAR_ATOM
  if (l == 1)			/* for 1 char strings: key = char */
    return (int) ((unsigned char) (*str));
#endif

  while (--p >= str)
    n = n * RADIX + hash_weight_tbl[(unsigned char) *p];

  n %= MAX_ATOM;

  return n;
}




/*-------------------------------------------------------------------------*
 * PL_GEN_NEW_ATOM                                                         *
 *                                                                         *
 * Find a new atom (gensym) beginning by a given prefix.                   *
 * hash<0 for any input or the index of the free atom to produce.          *
 *-------------------------------------------------------------------------*/
int
Pl_Gen_New_Atom(char *prefix, int hash)
{
  AtomInf *patom;

  if (pl_nb_atom == MAX_ATOM)
    Pl_Fatal_Error(ERR_ATOM_TBL_FULL);

  if (hash < 0)
    {
      patom = pl_atom_tbl;
      while (patom->name)
	patom++;
      hash = patom - pl_atom_tbl;
    }

  return Pl_Create_Allocate_Atom(Gen_Sym(prefix, hash));
}




/*-------------------------------------------------------------------------*
 * GEN_SYM                                                                 *
 *                                                                         *
 * returns a string beginning by a prefix st. its hash code is gen_sym_hash*
 *-------------------------------------------------------------------------*/
static char *
Gen_Sym(char *prefix, int gen_sym_hash)
{
  unsigned pl = strlen(prefix);
  unsigned hp = Hash_String(prefix);
  unsigned x, i;
  unsigned radix_p;
  char *str;

  strcpy(gen_sym_buff, prefix);
  str = gen_sym_buff + pl;

  x = (gen_sym_hash - hp) % MAX_ATOM;

  radix_p = 1;			/* compute 1/RADIX**pl = (1/RADIX)**pl */
  for (i = 0; i < pl; i++)
    radix_p *= INV_RADIX_MOD_MAX_ATOM;
  radix_p %= MAX_ATOM;


  x *= radix_p;			/* x = x/(RADIX**pl) */
  x %= MAX_ATOM;
  /* decompose x wrt radix RADIX */
  do
    {
      *str++ = hash_inv_tbl[x % RADIX];
      x /= RADIX;
    }
  while (x);

  *str = '\0';


#ifdef DEBUG
  if (Hash_String(gen_sym_buff) != gen_sym_hash)
    {
      DBGPRINTF("Gensym prefix: (%s) wanted hash: %d\n", prefix,
		gen_sym_hash);
      DBGPRINTF("   new string: (%s)    new hash: %d\n", gen_sym_buff,
		Hash_String(gen_sym_buff));
    }

#endif

  return gen_sym_buff;
}




/*-------------------------------------------------------------------------*
 * PL_FIND_NEXT_ATOM                                                       *
 *                                                                         *
 * returns the atom next after 'last_atom' (-1 to start) or -1 at the end  *
 *-------------------------------------------------------------------------*/
int
Pl_Find_Next_Atom(int last_atom)
{
  while (++last_atom < MAX_ATOM)
    {
      if (pl_atom_tbl[last_atom].name)
	return last_atom;
    }

  return -1;
}
