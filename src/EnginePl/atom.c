/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : atom.c                                                          *
 * Descr.: atom table management                                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2006 Daniel Diaz                                     *
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
#define INV_RADIX_MOD_MAX_ATOM     19563	/* see prog. euclide.c */




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

int char_type[256] = {

/*  nul soh stx etx eot enq ack bel bs  ht  nl  vt  np  cr  so  si  */
  LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA,

/*  dle dc1 dc2 dc3 dc4 nak syn etb can em sub esc  fs  gs  rs  us  */
  LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA, LA,

/*  spc !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /   */
  LA, SC, DQ, GR, GR, CM, GR, QT, PC, PC, GR, GR, SC, GR, GR, GR,

/*  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?   */
  DI, DI, DI, DI, DI, DI, DI, DI, DI, DI, GR, SC, GR, GR, GR, GR,

/*  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   */
  GR, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL,

/*  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _   */
  CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, CL, PC, GR, PC, GR, UL,

/*  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o    */
  BQ, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL,

/*  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~   del  */
  SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, SL, PC, PC, PC, GR, LA
/*  0x80 ... 0xff = EX (set by Init_Atom)"                           */
};


char escape_symbol[] = "abfnrtv";
char escape_char[] = "\a\b\f\n\r\t\v";


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
 * INIT_ATOM                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Init_Atom(void)
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

  for (c = 128; c < 256; c++) {
    if (isalpha (c)) {
      i++;
      char_type[c] = islower(c)? SL: CL;
      hash_weight_tbl[c] = i;
      hash_inv_tbl[i] = c;
    }
    else {
      char_type[c] = EX;	/* extended char set */
    }
  }

  for (i = 0; i < 256; i++)	/* initial conv mapping = identity */
    char_conv[i] = i;



  nb_atom = 0;

  for (i = 0; i < 256; i++)
    {
      str_char[i][0] = i;
      str_char[i][1] = '\0';
#ifndef OPTIM_1_CHAR_ATOM
      atom_char[i] =
#endif
	Create_Atom(str_char[i]);
    }

  i = Create_Atom("[]");
  if (i != ATOM_NIL)
    Fatal_Error(ERR_ATOM_NIL_INVALID, i);

  atom_void = Create_Atom("");
  atom_curly_brackets = Create_Atom("{}");

  atom_false = Create_Atom("false");
  atom_true = Create_Atom("true");

  atom_end_of_file = Create_Atom("end_of_file");
}




/*-------------------------------------------------------------------------*
 * CREATE_ALLOCATE_ATOM                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int FC
Create_Allocate_Atom(char *name)
{
  AtomInf *patom;
  char *name1;

  patom = Locate_Atom(name);

  if (patom == NULL)
    Fatal_Error(ERR_ATOM_TBL_FULL);

  if (patom->name != NULL)
    return patom - atom_tbl;	/* already exists */

  name1 = Strdup(name);

  return Create_Atom(name1);
}




/*-------------------------------------------------------------------------*
 * CREATE_ATOM                                                             *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
int FC
Create_Atom(char *name)
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
    Fatal_Error(ERR_ATOM_TBL_FULL);

  if (patom->name != NULL)
    return patom - atom_tbl;	/* already exists */

  nb_atom++;

  patom->name = name;
  prop.needs_scan = FALSE;

  identifier = graphic = (*name != '\0');

  for (p = name; *p; p++)
    {
      c_type = char_type[(unsigned char) *p];

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
    LE_Compl_Add_Word(name, lg);
#endif

  if (char_type[(unsigned char) *name] != SL)	/* small letter */
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
      prop.needs_quote = (lg == 1 && *name == '.');
      goto finish;
    }

  if (lg == 1 && char_type[(unsigned char) *name] == SC)
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

  return patom - atom_tbl;
}




/*-------------------------------------------------------------------------*
 * CREATE_ATOM                                                             *
 *                                                                         *
 * Called by compiled prolog code.                                         *
 *-------------------------------------------------------------------------*/
WamWord FC
Create_Atom_Tagged(char *name)
{
  return Tag_ATM(Create_Atom(name));
}




/*-------------------------------------------------------------------------*
 * FIND_ATOM                                                               *
 *                                                                         *
 * return the atom key or -1 if not exist.                                 *
 *-------------------------------------------------------------------------*/
int FC
Find_Atom(char *name)
{
  AtomInf *patom;

  patom = Locate_Atom(name);
  return (patom == NULL || patom->name == NULL) ? -1 : patom - atom_tbl;
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

  if (nb_atom == MAX_ATOM)
    return NULL;

  n = Hash_String(name);
  /* here either the atom is in the table */
  /* or there is at least one free cell.  */
  patom = atom_tbl + n;
  endt = atom_tbl + MAX_ATOM;

  while (patom->name && strcmp(patom->name, name) != 0)
    {
      patom++;
      if (patom == endt)
	patom = atom_tbl;
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
 * GEN_NEW_ATOM                                                            *
 *                                                                         *
 * Find a new atom (gensym) beginning by a given prefix.                   *
 * hash<0 for any input or the index of the free atom to produce.          *
 *-------------------------------------------------------------------------*/
int FC
Gen_New_Atom(char *prefix, int hash)
{
  AtomInf *patom;

  if (nb_atom == MAX_ATOM)
    Fatal_Error(ERR_ATOM_TBL_FULL);

  if (hash < 0)
    {
      patom = atom_tbl;
      while (patom->name)
	patom++;
      hash = patom - atom_tbl;
    }

  return Create_Allocate_Atom(Gen_Sym(prefix, hash));
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
 * FIND_NEXT_ATOM                                                          *
 *                                                                         *
 * returns the atom next after 'last_atom' (-1 to start) or -1 at the end  *
 *-------------------------------------------------------------------------*/
int FC
Find_Next_Atom(int last_atom)
{
  while (++last_atom < MAX_ATOM)
    {
      if (atom_tbl[last_atom].name)
	return last_atom;
    }

  return -1;
}
