/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : c_supp.c                                                        *
 * Descr.: C interface support                                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2002 Daniel Diaz                                     *
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

#include <string.h>

#include "engine_pl.h"
#include "bips_pl.h"




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

#define CHECK_FOR_UN_VARIABLE                                               \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
   if (tag_mask != TAG_REF_MASK)                                            \
     Pl_Err_Type(type_variable, word)


#define CHECK_FOR_UN_INTEGER                                                \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
   if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK)                \
     Pl_Err_Type(type_integer, word)


#define CHECK_FOR_UN_POSITIVE                                               \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK)                 \
    Pl_Err_Type(type_integer, word);                                        \
  if (tag_mask == TAG_INT_MASK && UnTag_INT(word)<0)                        \
    Pl_Err_Domain(domain_not_less_than_zero, word)


#define CHECK_FOR_UN_FLOAT                                                  \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_FLT_MASK)                 \
    Pl_Err_Type(type_float, word)


#define CHECK_FOR_UN_NUMBER                                                 \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK &&               \
      tag_mask != TAG_FLT_MASK)                                             \
     Pl_Err_Type(type_number, word)


#define CHECK_FOR_UN_ATOM                                                   \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK)                 \
    Pl_Err_Type(type_atom, word)


#define CHECK_FOR_UN_BOOLEAN                                                \
  WamWord word, tag_mask;                                                   \
  int atom;                                                                 \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  atom = UnTag_ATM(word);                                                   \
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_ATM_MASK ||              \
      (atom != atom_true && atom != atom_false)))                           \
     Pl_Err_Type(type_boolean, word)


#define CHECK_FOR_UN_CHAR                                                   \
  WamWord word, tag_mask;                                                   \
  int atom;                                                                 \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  atom = UnTag_ATM(word);                                                   \
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_ATM_MASK ||              \
      atom_tbl[atom].prop.length != 1))                                     \
    Pl_Err_Type(type_character, word)


#define CHECK_FOR_UN_IN_CHAR                                                \
  WamWord word, tag_mask;                                                   \
  int atom;                                                                 \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  atom = UnTag_ATM(word);                                                   \
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_ATM_MASK ||              \
      (atom != atom_end_of_file && atom_tbl[atom].prop.length != 1)))       \
    Pl_Err_Type(type_in_character, word)


#define CHECK_FOR_UN_CODE                                                   \
  WamWord word, tag_mask;                                                   \
  int c;                                                                    \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK)                 \
    Pl_Err_Type(type_integer, word);                                        \
  c = UnTag_INT(word);                                                      \
  if (tag_mask == TAG_INT_MASK && !Is_Valid_Code(c))                        \
    Pl_Err_Representation(representation_character_code)


#define CHECK_FOR_UN_IN_CODE                                                \
  WamWord word, tag_mask;                                                   \
  int c;                                                                    \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK)                 \
    Pl_Err_Type(type_integer, word);                                        \
  c = UnTag_INT(word);                                                      \
  if (tag_mask == TAG_INT_MASK && c != -1 && !Is_Valid_Code(c))             \
    Pl_Err_Representation(representation_in_character_code)


#define CHECK_FOR_UN_BYTE                                                   \
  WamWord word, tag_mask;                                                   \
  int c;                                                                    \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  c = UnTag_INT(word);                                                      \
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_INT_MASK ||              \
      !Is_Valid_Byte(c)))                                                   \
    Pl_Err_Type(type_byte, word)


#define CHECK_FOR_UN_IN_BYTE                                                \
  WamWord word, tag_mask;                                                   \
  int c;                                                                    \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  c = UnTag_INT(word);                                                      \
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_INT_MASK ||              \
      (c != -1 && !Is_Valid_Byte(c))))                                      \
     Pl_Err_Type(type_in_byte, word)


#define CHECK_FOR_UN_COMPOUND                                               \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_LST_MASK &&               \
      tag_mask != TAG_STC_MASK)                                             \
    Pl_Err_Type(type_compound, word)


#define CHECK_FOR_UN_CALLABLE                                               \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK &&               \
      tag_mask != TAG_LST_MASK && tag_mask != TAG_STC_MASK)                 \
    Pl_Err_Type(type_callable, word)




/*-------------------------------------------------------------------------*
 * RD_INTEGER_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
long
Rd_Integer_Check(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_INT_MASK)
    Pl_Err_Type(type_integer, word);

  return UnTag_INT(word);
}




/*-------------------------------------------------------------------------*
 * RD_INTEGER                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
long
Rd_Integer(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  return UnTag_INT(word);
}




/*-------------------------------------------------------------------------*
 * RD_POSITIVE_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
long
Rd_Positive_Check(WamWord start_word)
{
  long n = Rd_Integer_Check(start_word);

  if (n < 0)
    Pl_Err_Domain(domain_not_less_than_zero, start_word);

  return n;
}




/*-------------------------------------------------------------------------*
 * RD_POSITIVE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
long
Rd_Positive(WamWord start_word)
{
  return Rd_Integer(start_word);
}




/*-------------------------------------------------------------------------*
 * RD_FLOAT_CHECK                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
double
Rd_Float_Check(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_FLT_MASK)
    Pl_Err_Type(type_float, word);

  return Obtain_Float(UnTag_FLT(word));
}




/*-------------------------------------------------------------------------*
 * RD_FLOAT                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
double
Rd_Float(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  return Obtain_Float(UnTag_FLT(word));
}




/*-------------------------------------------------------------------------*
 * RD_NUMBER_CHECK                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
double
Rd_Number_Check(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_FLT_MASK && tag_mask != TAG_INT_MASK)
    Pl_Err_Type(type_number, word);

  if (tag_mask == TAG_INT_MASK)
    return (double) UnTag_INT(word);

  return Obtain_Float(UnTag_FLT(word));
}




/*-------------------------------------------------------------------------*
 * RD_NUMBER                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
double
Rd_Number(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  if (tag_mask == TAG_INT_MASK)
    return (double) UnTag_INT(word);

  return Obtain_Float(UnTag_FLT(word));
}




/*-------------------------------------------------------------------------*
 * RD_ATOM_CHECK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Atom_Check(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_ATM_MASK)
    Pl_Err_Type(type_atom, word);

  return UnTag_ATM(word);
}




/*-------------------------------------------------------------------------*
 * RD_ATOM                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Atom(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  return UnTag_ATM(word);
}




/*-------------------------------------------------------------------------*
 * RD_BOOLEAN_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Boolean_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  atom = UnTag_ATM(word);
  if (tag_mask != TAG_ATM_MASK || (atom != atom_true && atom != atom_false))
    Pl_Err_Type(type_boolean, word);

  return atom != atom_false;
}




/*-------------------------------------------------------------------------*
 * RD_BOOLEAN                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Boolean(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);

  atom = UnTag_ATM(word);

  return atom != atom_false;
}




/*-------------------------------------------------------------------------*
 * RD_CHAR_CHECK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Char_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  atom = UnTag_ATM(word);
  if (tag_mask != TAG_ATM_MASK || atom_tbl[atom].prop.length != 1)
    Pl_Err_Type(type_character, word);

  return atom_tbl[atom].name[0];
}




/*-------------------------------------------------------------------------*
 * RD_CHAR                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Char(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);
  atom = UnTag_ATM(word);

  return atom_tbl[atom].name[0];
}




/*-------------------------------------------------------------------------*
 * RD_IN_CHAR_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_In_Char_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  atom = UnTag_ATM(word);
  if (tag_mask != TAG_ATM_MASK || 
      (atom != atom_end_of_file && atom_tbl[atom].prop.length != 1))
    Pl_Err_Type(type_in_character, word);

  return (atom != atom_end_of_file) ? atom_tbl[atom].name[0] : -1;
}




/*-------------------------------------------------------------------------*
 * RD_IN_CHAR                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_In_Char(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);
  atom = UnTag_ATM(word);
  return (atom != atom_end_of_file) ? atom_tbl[atom].name[0] : -1;
}




/*-------------------------------------------------------------------------*
 * RD_CODE_CHECK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Code_Check(WamWord start_word)
{
  int c;

  c = Rd_Integer_Check(start_word);
  if (!Is_Valid_Code(c))
    Pl_Err_Representation(representation_character_code);

  return c;
}




/*-------------------------------------------------------------------------*
 * RD_CODE                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Code(WamWord start_word)
{
  return Rd_Integer(start_word);
}




/*-------------------------------------------------------------------------*
 * RD_IN_CODE_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_In_Code_Check(WamWord start_word)
{
  int c;

  c = Rd_Integer_Check(start_word);
  if (c != -1 && !Is_Valid_Code(c))
    Pl_Err_Representation(representation_in_character_code);

  return c;
}




/*-------------------------------------------------------------------------*
 * RD_IN_CODE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_In_Code(WamWord start_word)
{
  return Rd_Integer(start_word);
}




/*-------------------------------------------------------------------------*
 * RD_BYTE_CHECK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Byte_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  int c;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  c = UnTag_INT(word);
  if (tag_mask != TAG_INT_MASK || !Is_Valid_Byte(c))
    Pl_Err_Type(type_byte, word);

  return c;
}




/*-------------------------------------------------------------------------*
 * RD_BYTE                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Byte(WamWord start_word)
{
  return Rd_Integer(start_word);
}




/*-------------------------------------------------------------------------*
 * RD_IN_BYTE_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_In_Byte_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  int c;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  c = UnTag_INT(word);
  if (tag_mask != TAG_INT_MASK || (c != -1 && !Is_Valid_Byte(c)))
    Pl_Err_Type(type_in_byte, word);

  return c;
}




/*-------------------------------------------------------------------------*
 * RD_IN_BYTE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_In_Byte(WamWord start_word)
{
  return Rd_Integer(start_word);
}




/*-------------------------------------------------------------------------*
 * RD_STRING_CHECK                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Rd_String_Check(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_ATM_MASK)
    Pl_Err_Type(type_atom, word);

  return atom_tbl[UnTag_ATM(word)].name;
}




/*-------------------------------------------------------------------------*
 * RD_STRING                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Rd_String(WamWord start_word)
{
  return atom_tbl[Rd_Atom(start_word)].name;
}




/*-------------------------------------------------------------------------*
 * RD_CHARS_CHECK                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Rd_Chars_Check(WamWord start_word)
{
  Rd_Chars_Str_Check(start_word, glob_buff);
  return glob_buff;
}




/*-------------------------------------------------------------------------*
 * RD_CHARS                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Rd_Chars(WamWord start_word)
{
  Rd_Chars_Str(start_word, glob_buff);
  return glob_buff;
}




/*-------------------------------------------------------------------------*
 * RD_CODES_CHECK                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Rd_Codes_Check(WamWord start_word)
{
  Rd_Codes_Str_Check(start_word, glob_buff);
  return glob_buff;
}




/*-------------------------------------------------------------------------*
 * RD_CODES                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Rd_Codes(WamWord start_word)
{
  Rd_Codes_Str(start_word, glob_buff);
  return glob_buff;
}




/*-------------------------------------------------------------------------*
 * RD_CHARS_STR_CHECK                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Chars_Str_Check(WamWord start_word, char *str)
{
  WamWord word, tag_mask;
  WamWord save_start_word;
  WamWord *lst_adr;
  int n = 0;

  save_start_word = start_word;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(type_list, save_start_word);

      lst_adr = UnTag_LST(word);

      *str++ = Rd_Char_Check(Car(lst_adr));
      n++;

      start_word = Cdr(lst_adr);
    }

  *str = '\0';
  return n;
}




/*-------------------------------------------------------------------------*
 * RD_CHARS_STR                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Chars_Str(WamWord start_word, char *str)
{
  WamWord word, tag_mask;
  WamWord save_start_word;
  WamWord *lst_adr;
  int n = 0;

  save_start_word = start_word;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (word == NIL_WORD)
	break;

      lst_adr = UnTag_LST(word);

      *str++ = Rd_Char_Check(Car(lst_adr));
      n++;

      start_word = Cdr(lst_adr);
    }

  *str = '\0';
  return n;
}




/*-------------------------------------------------------------------------*
 * RD_CODES_STR_CHECK                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Codes_Str_Check(WamWord start_word, char *str)
{
  WamWord word, tag_mask;
  WamWord save_start_word;
  WamWord *lst_adr;
  int n = 0;

  save_start_word = start_word;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(type_list, save_start_word);

      lst_adr = UnTag_LST(word);

      *str++ = Rd_Code_Check(Car(lst_adr));
      n++;

      start_word = Cdr(lst_adr);
    }

  *str = '\0';
  return n;
}




/*-------------------------------------------------------------------------*
 * RD_CODES_STR                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Codes_Str(WamWord start_word, char *str)
{
  WamWord word, tag_mask;
  WamWord save_start_word;
  WamWord *lst_adr;
  int n = 0;

  save_start_word = start_word;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (word == NIL_WORD)
	break;

      lst_adr = UnTag_LST(word);

      *str++ = Rd_Code_Check(Car(lst_adr));
      n++;

      start_word = Cdr(lst_adr);
    }

  *str = '\0';
  return n;
}




/*-------------------------------------------------------------------------*
 * RD_LIST_CHECK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Rd_List_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *lst_adr;

  DEREF(start_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (word == NIL_WORD)
    return NULL;

  if (tag_mask != TAG_LST_MASK)
    Pl_Err_Type(type_list, start_word);

  lst_adr = UnTag_LST(word);
  return &Car(lst_adr);
}




/*-------------------------------------------------------------------------*
 * RD_LIST                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Rd_List(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *lst_adr;

  DEREF(start_word, word, tag_mask);

  if (word == NIL_WORD)
    return NULL;

  lst_adr = UnTag_LST(word);
  return &Car(lst_adr);
}




/*-------------------------------------------------------------------------*
 * RD_PROPER_LIST_CHECK                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Proper_List_Check(WamWord start_word, WamWord *arg)
{
  WamWord word, tag_mask;
  WamWord save_start_word;
  WamWord *lst_adr;
  int n = 0;

  save_start_word = start_word;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(type_list, save_start_word);

      lst_adr = UnTag_LST(word);

      *arg++ = Car(lst_adr);
      n++;

      start_word = Cdr(lst_adr);
    }

  return n;
}




/*-------------------------------------------------------------------------*
 * RD_PROPER_LIST                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Rd_Proper_List(WamWord start_word, WamWord *arg)
{
  WamWord word, tag_mask;
  WamWord save_start_word;
  WamWord *lst_adr;
  int n = 0;

  save_start_word = start_word;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (word == NIL_WORD)
	break;

      lst_adr = UnTag_LST(word);

      *arg++ = Car(lst_adr);
      n++;

      start_word = Cdr(lst_adr);
    }

  return n;
}




/*-------------------------------------------------------------------------*
 * RD_COMPOUND_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Rd_Compound_Check(WamWord start_word, int *func, int *arity)
{
  WamWord word, tag_mask;
  WamWord *adr;

  DEREF(start_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask == TAG_LST_MASK)
    {
      adr = UnTag_LST(word);
      *func = ATOM_CHAR('.');
      *arity = 2;
      return &Car(adr);
    }

  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);
      *func = Functor(adr);
      *arity = Arity(adr);
      return &Arg(adr, 0);
    }

  Pl_Err_Type(type_compound, start_word);
  return NULL;
}




/*-------------------------------------------------------------------------*
 * RD_COMPOUND                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Rd_Compound(WamWord start_word, int *func, int *arity)
{
  WamWord word, tag_mask;
  WamWord *adr;

  DEREF(start_word, word, tag_mask);

  if (tag_mask == TAG_LST_MASK)
    {
      adr = UnTag_LST(word);
      *func = ATOM_CHAR('.');
      *arity = 2;
      return &Car(adr);
    }

  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);
      *func = Functor(adr);
      *arity = Arity(adr);
      return &Arg(adr, 0);
    }

  return NULL;
}




/*-------------------------------------------------------------------------*
 * RD_CALLABLE_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Rd_Callable_Check(WamWord start_word, int *func, int *arity)
{
  WamWord word, tag_mask;
  WamWord *adr;

  DEREF(start_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();


  if (tag_mask == TAG_ATM_MASK)
    {
      *func = UnTag_ATM(word);
      *arity = 0;
      return NULL;
    }

  if (tag_mask == TAG_LST_MASK)
    {
      adr = UnTag_LST(word);
      *func = ATOM_CHAR('.');
      *arity = 2;
      return &Car(adr);
    }

  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);
      *func = Functor(adr);
      *arity = Arity(adr);
      return &Arg(adr, 0);
    }

  Pl_Err_Type(type_callable, start_word);
  return NULL;
}




/*-------------------------------------------------------------------------*
 * RD_CALLABLE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Rd_Callable(WamWord start_word, int *func, int *arity)
{
  WamWord word, tag_mask;
  WamWord *adr;

  DEREF(start_word, word, tag_mask);

  if (tag_mask == TAG_ATM_MASK)
    {
      *func = UnTag_ATM(word);
      *arity = 0;
      return (WamWord *) arity;	/* anything except NULL */
    }

  if (tag_mask == TAG_LST_MASK)
    {
      adr = UnTag_LST(word);
      *func = ATOM_CHAR('.');
      *arity = 2;
      return &Car(adr);
    }

  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);
      *func = Functor(adr);
      *arity = Arity(adr);
      return &Arg(adr, 0);
    }

  return NULL;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_INTEGER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Integer(WamWord start_word)
{
  CHECK_FOR_UN_INTEGER;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_POSITIVE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Positive(WamWord start_word)
{
  CHECK_FOR_UN_POSITIVE;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_FLOAT                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Float(WamWord start_word)
{
  CHECK_FOR_UN_FLOAT;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_NUMBER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Number(WamWord start_word)
{
  CHECK_FOR_UN_NUMBER;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_ATOM                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Atom(WamWord start_word)
{
  CHECK_FOR_UN_ATOM;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_BOOLEAN                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Boolean(WamWord start_word)
{
  CHECK_FOR_UN_BOOLEAN;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_CHAR                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Char(WamWord start_word)
{
  CHECK_FOR_UN_CHAR;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_IN_CHAR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_In_Char(WamWord start_word)
{
  CHECK_FOR_UN_IN_CHAR;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_CODE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Code(WamWord start_word)
{
  CHECK_FOR_UN_CODE;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_IN_CODE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_In_Code(WamWord start_word)
{
  CHECK_FOR_UN_IN_CODE;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_BYTE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Byte(WamWord start_word)
{
  CHECK_FOR_UN_BYTE;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_IN_BYTE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_In_Byte(WamWord start_word)
{
  CHECK_FOR_UN_IN_BYTE;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_CHARS                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Chars(WamWord start_word)
{
  Check_For_Un_List(start_word);
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_STRING                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_String(WamWord start_word)
{
  Check_For_Un_Atom(start_word);
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_CODES                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Codes(WamWord start_word)
{
  Check_For_Un_List(start_word);
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_LIST                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_List(WamWord start_word)
{
  if (!Blt_List_Or_Partial_List(start_word))
    Pl_Err_Type(type_list, start_word);
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_COMPOUND                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Compound(WamWord start_word)
{
  CHECK_FOR_UN_COMPOUND;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_CALLABLE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Callable(WamWord start_word)
{
  CHECK_FOR_UN_CALLABLE;
}




/*-------------------------------------------------------------------------*
 * CHECK_FOR_UN_VARIABLE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Check_For_Un_Variable(WamWord start_word)
{
  CHECK_FOR_UN_VARIABLE;
}




/*-------------------------------------------------------------------------*
 * UN_INTEGER_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Integer_Check(long value, WamWord start_word)
{
  CHECK_FOR_UN_INTEGER;

  return Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * UN_INTEGER                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Integer(long value, WamWord start_word)
{
  return Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_POSITIVE_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Positive_Check(long value, WamWord start_word)
{
  CHECK_FOR_UN_POSITIVE;

  return Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * UN_POSITIVE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Positive(long value, WamWord start_word)
{
  return Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_FLOAT_CHECK                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Float_Check(double value, WamWord start_word)
{
  CHECK_FOR_UN_FLOAT;

  return Get_Float(value, word);
}




/*-------------------------------------------------------------------------*
 * UN_FLOAT                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Float(double value, WamWord start_word)
{
  return Get_Float(value, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_NUMBER_CHECK                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Number_Check(double value, WamWord start_word)
{
  long n;

  CHECK_FOR_UN_NUMBER;

  n = (long) value;

  return (n == value) ? Get_Integer(n, word) : Get_Float(value, word);
}




/*-------------------------------------------------------------------------*
 * UN_NUMBER                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Number(double value, WamWord start_word)
{
  long n;

  n = (long) value;

  return (n == value) ? Get_Integer(n, start_word) :
    Get_Float(value, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_ATOM_CHECK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Atom_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_ATOM;

  return Get_Atom(value, word);
}




/*-------------------------------------------------------------------------*
 * UN_ATOM                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Atom(int value, WamWord start_word)
{
  return Get_Atom(value, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_BOOLEAN_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Boolean_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_BOOLEAN;

  return Get_Atom(value ? atom_true : atom_false, word);
}




/*-------------------------------------------------------------------------*
 * UN_BOOLEAN                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Boolean(int value, WamWord start_word)
{
  return Get_Atom(value ? atom_true : atom_false, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_CHAR_CHECK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Char_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_CHAR;

  return Get_Atom(ATOM_CHAR(value), word);
}




/*-------------------------------------------------------------------------*
 * UN_CHAR                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Char(int value, WamWord start_word)
{
  return Get_Atom(ATOM_CHAR(value), start_word);
}




/*-------------------------------------------------------------------------*
 * UN_IN_CHAR_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_In_Char_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_IN_CHAR;

  return Get_Atom((value == -1) ? atom_end_of_file : ATOM_CHAR(value),
		  word);
}




/*-------------------------------------------------------------------------*
 * UN_IN_CHAR                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_In_Char(int value, WamWord start_word)
{
  return Get_Atom((value == -1) ? atom_end_of_file : ATOM_CHAR(value),
		  start_word);
}




/*-------------------------------------------------------------------------*
 * UN_CODE_CHECK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Code_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_CODE;

  return Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * UN_CODE                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Code(int value, WamWord start_word)
{
  return Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_IN_CODE_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_In_Code_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_IN_CODE;

  return Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * UN_IN_CODE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_In_Code(int value, WamWord start_word)
{
  return Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_BYTE_CHECK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Byte_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_BYTE;

  return Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * UN_BYTE                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Byte(int value, WamWord start_word)
{
  return Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_IN_BYTE_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_In_Byte_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_IN_BYTE;

  return Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * UN_IN_BYTE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_In_Byte(int value, WamWord start_word)
{
  return Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_STRING_CHECK                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_String_Check(char *value, WamWord start_word)
{
  CHECK_FOR_UN_ATOM;

  return Get_Atom(Create_Allocate_Atom(value), word);
}




/*-------------------------------------------------------------------------*
 * UN_STRING                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_String(char *value, WamWord start_word)
{
  return Get_Atom(Create_Allocate_Atom(value), start_word);
}




/*-------------------------------------------------------------------------*
 * UN_CHARS_CHECK                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Chars_Check(char *str, WamWord start_word)
{
  Check_For_Un_List(start_word);

  return Un_Chars(str, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_CHARS                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Chars(char *str, WamWord start_word)
{
  for (; *str; str++)
    {
      if (!Get_List(start_word) || !Unify_Atom(ATOM_CHAR(*str)))
	return FALSE;

      start_word = Unify_Variable();
    }

  return Get_Nil(start_word);
}




/*-------------------------------------------------------------------------*
 * UN_CODES_CHECK                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Codes_Check(char *str, WamWord start_word)
{
  Check_For_Un_List(start_word);

  return Un_Codes(str, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_CODES                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Codes(char *str, WamWord start_word)
{
  for (; *str; str++)
    {
      if (!Get_List(start_word) || !Unify_Integer(*str))
	return FALSE;

      start_word = Unify_Variable();
    }

  return Get_Nil(start_word);
}




/*-------------------------------------------------------------------------*
 * UN_LIST_CHECK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_List_Check(WamWord *arg, WamWord start_word)
{
  WamWord word, tag_mask;

  if (arg == NULL)
    {
      if (Get_Nil(start_word))
	return TRUE;

    check_type:
      DEREF(start_word, word, tag_mask);
      if (word != NIL_WORD && tag_mask != TAG_LST_MASK)
	Pl_Err_Type(type_list, start_word);

      return FALSE;
    }

  if (!Get_List(start_word))
    goto check_type;

  return Unify_Value(arg[0]) && Unify_Value(arg[1]);
}




/*-------------------------------------------------------------------------*
 * UN_LIST                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_List(WamWord *arg, WamWord start_word)
{
  if (arg == NULL)
    return Get_Nil(start_word);

  return Get_List(start_word) && Unify_Value(arg[0]) && Unify_Value(arg[1]);
}




/*-------------------------------------------------------------------------*
 * UN_PROPER_LIST_CHECK                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Proper_List_Check(int n, WamWord *arg, WamWord start_word)
{
  Check_For_Un_List(start_word);

  return Un_Proper_List(n, arg, start_word);
}




/*-------------------------------------------------------------------------*
 * UN_PROPER_LIST                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Proper_List(int n, WamWord *arg, WamWord start_word)
{
  if (n < 0 || arg == NULL)
    n = 0;

  while (n--)
    {
      if (!Get_List(start_word) || !Unify_Value(*arg++))
	return FALSE;

      start_word = Unify_Variable();
    }

  return Get_Nil(start_word);
}




/*-------------------------------------------------------------------------*
 * UN_COMPOUND_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Compound_Check(int func, int arity, WamWord *arg, WamWord start_word)
{
  int i;

  if (arity == 0)
    return Un_Atom_Check(func, start_word);

  if (arity == 2 && func == ATOM_CHAR('.'))
    return Un_List_Check(arg, start_word);

  if (!Get_Structure(func, arity, start_word))
    {
      if (!Blt_Compound(start_word))
	Pl_Err_Type(type_compound, start_word);
      return FALSE;
    }

  for (i = 0; i < arity; i++)
    if (!Unify_Value(arg[i]))
      return FALSE;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * UN_COMPOUND                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Compound(int func, int arity, WamWord *arg, WamWord start_word)
{
  int i;

  if (arity == 0)
    return Un_Atom_Check(func, start_word);

  if (arity == 2 && func == ATOM_CHAR('.'))
    return Un_List(arg, start_word);

  if (!Get_Structure(func, arity, start_word))
    return FALSE;

  for (i = 0; i < arity; i++)
    if (!Unify_Value(arg[i]))
      return FALSE;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * UN_CALLABLE_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Callable_Check(int func, int arity, WamWord *arg, WamWord start_word)
{
  int i;

  if (arity == 0)
    return Un_Atom_Check(func, start_word);

  if (arity == 2 && func == ATOM_CHAR('.'))
    return Un_List_Check(arg, start_word);

  if (!Get_Structure(func, arity, start_word))
    {
      if (!Blt_Callable(start_word))
	Pl_Err_Type(type_callable, start_word);
      return FALSE;
    }

  for (i = 0; i < arity; i++)
    if (!Unify_Value(arg[i]))
      return FALSE;

  return TRUE;
}



/*-------------------------------------------------------------------------*
 * UN_CALLABLE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Un_Callable(int func, int arity, WamWord *arg, WamWord start_word)
{
  return Un_Compound(func, arity, arg, start_word);
}




/*-------------------------------------------------------------------------*
 * MK_INTEGER                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Integer(long value)
{
  return Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * MK_POSITIVE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Positive(long value)
{
  return Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * MK_FLOAT                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Float(double value)
{
  return Put_Float(value);
}




/*-------------------------------------------------------------------------*
 * MK_NUMBER                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Number(double value)
{
  int n;

  n = (long) value;

  if (n == value)
    return Put_Integer(n);

  return Put_Float(value);
}




/*-------------------------------------------------------------------------*
 * MK_ATOM                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Atom(int value)
{
  return Put_Atom(value);
}




/*-------------------------------------------------------------------------*
 * MK_BOOLEAN                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Boolean(int value)
{
  return Put_Atom(value ? atom_true : atom_false);
}




/*-------------------------------------------------------------------------*
 * MK_CHAR                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Char(int value)
{
  return Put_Atom(ATOM_CHAR(value));
}




/*-------------------------------------------------------------------------*
 * MK_IN_CHAR                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_In_Char(int value)
{
  return Put_Atom((value == -1) ? atom_end_of_file : ATOM_CHAR(value));
}




/*-------------------------------------------------------------------------*
 * MK_CODE                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Code(int value)
{
  return Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * MK_IN_CODE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_In_Code(int value)
{
  return Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * MK_BYTE                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Byte(int value)
{
  return Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * MK_IN_BYTE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_In_Byte(int value)
{
  return Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * MK_STRING                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_String(char *value)
{
  return Put_Atom(Create_Allocate_Atom(value));
}




/*-------------------------------------------------------------------------*
 * MK_CHARS                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Chars(char *str)
{
  WamWord res_word;

  if (*str == '\0')
    return NIL_WORD;

  res_word = Put_List();
  for (;;)
    {
      Unify_Atom(ATOM_CHAR(*str));
      str++;
      if (*str == '\0')
	break;

      Unify_List();
    }

  Unify_Nil();
  return res_word;
}




/*-------------------------------------------------------------------------*
 * MK_CODES                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Codes(char *str)
{
  WamWord res_word;

  if (*str == '\0')
    return NIL_WORD;

  res_word = Put_List();
  for (;;)
    {
      Unify_Integer(*str);
      str++;
      if (*str == '\0')
	break;

      Unify_List();
    }

  Unify_Nil();
  return res_word;
}




/*-------------------------------------------------------------------------*
 * MK_LIST                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_List(WamWord *arg)
{
  WamWord res_word;

  if (arg == NULL)
    return NIL_WORD;

  res_word = Put_List();
  Unify_Value(arg[0]);
  Unify_Value(arg[1]);

  return res_word;
}








/*-------------------------------------------------------------------------*
 * MK_PROPER_LIST                                                          *
 *                                                                         *
 * This function transform an array of n WamWords located at arg into a    *
 * Prolog list (pushed at the top of the heap) and returns the resulting   *
 * list word.                                                              *
 * Note: arg can be equal to H to tranform an array into a list in-place.  *
 * The resulting list uses 2*n WamWord from the top of the heap.           *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Proper_List(int n, WamWord *arg)
{
  WamWord *src, *dst, *p;

  if (n <= 0 || arg == NULL)
    return NIL_WORD;

  src = arg + n;		/* copy from end to start to make possible */
  dst = H = H + 2 * n;		/* in-place array->list transformation     */

  *--dst = NIL_WORD;
  goto entry;

  do
    {
      p = dst--;
      *dst = Tag_LST(p);
    entry:
      *--dst = *--src;
    }
  while (--n);

  return Tag_LST(dst);
}




/*-------------------------------------------------------------------------*
 * MK_COMPOUND                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Compound(int func, int arity, WamWord *arg)
{
  WamWord res_word;
  int i;

  if (arity == 0)
    return Put_Atom(func);

  if (arity == 2 && func == ATOM_CHAR('.'))
    return Mk_List(arg);

  res_word = Put_Structure(func, arity);
  for (i = 0; i < arity; i++)
    Unify_Value(arg[i]);

  return res_word;
}




/*-------------------------------------------------------------------------*
 * MK_CALLABLE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Callable(int func, int arity, WamWord *arg)
{
  return Mk_Compound(func, arity, arg);
}




/*-------------------------------------------------------------------------*
 * MK_VARIABLE                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Mk_Variable(void)
{
  return Put_X_Variable();
}
