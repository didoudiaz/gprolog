/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : c_supp.c                                                        *
 * Descr.: C interface support                                             *
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
     Pl_Err_Uninstantiation(word)


#define CHECK_FOR_UN_INTEGER                                                \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
   if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK)                \
     Pl_Err_Type(pl_type_integer, word)


#define CHECK_FOR_UN_POSITIVE                                               \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK)                 \
    Pl_Err_Type(pl_type_integer, word);                                     \
  if (tag_mask == TAG_INT_MASK && UnTag_INT(word)<0)                        \
    Pl_Err_Domain(pl_domain_not_less_than_zero, word)


#define CHECK_FOR_UN_FLOAT                                                  \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_FLT_MASK)                 \
    Pl_Err_Type(pl_type_float, word)


#define CHECK_FOR_UN_NUMBER                                                 \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK &&               \
      tag_mask != TAG_FLT_MASK)                                             \
     Pl_Err_Type(pl_type_number, word)


#define CHECK_FOR_UN_ATOM                                                   \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK)                 \
    Pl_Err_Type(pl_type_atom, word)


#define CHECK_FOR_UN_BOOLEAN                                                \
  WamWord word, tag_mask;                                                   \
  int atom;                                                                 \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  atom = UnTag_ATM(word);                                                   \
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_ATM_MASK ||              \
      (atom != pl_atom_true && atom != pl_atom_false)))                     \
     Pl_Err_Type(pl_type_boolean, word)


#define CHECK_FOR_UN_CHAR                                                   \
  WamWord word, tag_mask;                                                   \
  int atom;                                                                 \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  atom = UnTag_ATM(word);                                                   \
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_ATM_MASK ||              \
      pl_atom_tbl[atom].prop.length != 1))                                  \
    Pl_Err_Type(pl_type_character, word)


#define CHECK_FOR_UN_IN_CHAR                                                \
  WamWord word, tag_mask;                                                   \
  int atom;                                                                 \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  atom = UnTag_ATM(word);                                                   \
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_ATM_MASK ||              \
      (atom != pl_atom_end_of_file && pl_atom_tbl[atom].prop.length != 1))) \
    Pl_Err_Type(pl_type_in_character, word)


#define CHECK_FOR_UN_CODE                                                   \
  WamWord word, tag_mask;                                                   \
  PlLong c;                                                                 \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK)                 \
    Pl_Err_Type(pl_type_integer, word);                                     \
  c = UnTag_INT(word);                                                      \
  if (tag_mask == TAG_INT_MASK && !Is_Valid_Code(c))                        \
    Pl_Err_Representation(pl_representation_character_code)


#define CHECK_FOR_UN_IN_CODE                                                \
  WamWord word, tag_mask;                                                   \
  PlLong c;                                                                 \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK)                 \
    Pl_Err_Type(pl_type_integer, word);                                     \
  c = UnTag_INT(word);                                                      \
  if (tag_mask == TAG_INT_MASK && c != -1 && !Is_Valid_Code(c))             \
    Pl_Err_Representation(pl_representation_in_character_code)


#define CHECK_FOR_UN_BYTE                                                   \
  WamWord word, tag_mask;                                                   \
  PlLong c;                                                                 \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  c = UnTag_INT(word);                                                      \
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_INT_MASK ||              \
      !Is_Valid_Byte(c)))                                                   \
    Pl_Err_Type(pl_type_byte, word)


#define CHECK_FOR_UN_IN_BYTE                                                \
  WamWord word, tag_mask;                                                   \
  PlLong c;                                                                 \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  c = UnTag_INT(word);                                                      \
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_INT_MASK ||              \
      (c != -1 && !Is_Valid_Byte(c))))                                      \
     Pl_Err_Type(pl_type_in_byte, word)


#define CHECK_FOR_UN_COMPOUND                                               \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_LST_MASK &&               \
      tag_mask != TAG_STC_MASK)                                             \
    Pl_Err_Type(pl_type_compound, word)


#define CHECK_FOR_UN_CALLABLE                                               \
  WamWord word, tag_mask;                                                   \
                                                                            \
  DEREF(start_word, word, tag_mask);                                        \
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK &&               \
      tag_mask != TAG_LST_MASK && tag_mask != TAG_STC_MASK)                 \
    Pl_Err_Type(pl_type_callable, word)




/*-------------------------------------------------------------------------*
 * PL_RD_INTEGER_CHECK                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlLong
Pl_Rd_Integer_Check(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_INT_MASK)
    Pl_Err_Type(pl_type_integer, word);

  return UnTag_INT(word);
}




/*-------------------------------------------------------------------------*
 * PL_RD_INTEGER                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlLong
Pl_Rd_Integer(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  return UnTag_INT(word);
}




/*-------------------------------------------------------------------------*
 * PL_RD_POSITIVE_CHECK                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlLong
Pl_Rd_Positive_Check(WamWord start_word)
{
  PlLong n = Pl_Rd_Integer_Check(start_word);

  if (n < 0)
    Pl_Err_Domain(pl_domain_not_less_than_zero, start_word);

  return n;
}




/*-------------------------------------------------------------------------*
 * PL_RD_POSITIVE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlLong
Pl_Rd_Positive(WamWord start_word)
{
  return Pl_Rd_Integer(start_word);
}




/*-------------------------------------------------------------------------*
 * PL_RD_FLOAT_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
double
Pl_Rd_Float_Check(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_FLT_MASK)
    Pl_Err_Type(pl_type_float, word);

  return Pl_Obtain_Float(UnTag_FLT(word));
}




/*-------------------------------------------------------------------------*
 * PL_RD_FLOAT                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
double
Pl_Rd_Float(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  return Pl_Obtain_Float(UnTag_FLT(word));
}




/*-------------------------------------------------------------------------*
 * PL_RD_NUMBER_CHECK                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
double
Pl_Rd_Number_Check(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_FLT_MASK && tag_mask != TAG_INT_MASK)
    Pl_Err_Type(pl_type_number, word);

  if (tag_mask == TAG_INT_MASK)
    return (double) UnTag_INT(word);

  return Pl_Obtain_Float(UnTag_FLT(word));
}




/*-------------------------------------------------------------------------*
 * PL_RD_NUMBER                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
double
Pl_Rd_Number(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  if (tag_mask == TAG_INT_MASK)
    return (double) UnTag_INT(word);

  return Pl_Obtain_Float(UnTag_FLT(word));
}




/*-------------------------------------------------------------------------*
 * PL_RD_ATOM_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Atom_Check(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_ATM_MASK)
    Pl_Err_Type(pl_type_atom, word);

  return UnTag_ATM(word);
}




/*-------------------------------------------------------------------------*
 * PL_RD_ATOM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Atom(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  return UnTag_ATM(word);
}




/*-------------------------------------------------------------------------*
 * PL_RD_BOOLEAN_CHECK                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Boolean_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  atom = UnTag_ATM(word);
  if (tag_mask != TAG_ATM_MASK || (atom != pl_atom_true && atom != pl_atom_false))
    Pl_Err_Type(pl_type_boolean, word);

  return atom != pl_atom_false;
}




/*-------------------------------------------------------------------------*
 * PL_RD_BOOLEAN                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Boolean(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);

  atom = UnTag_ATM(word);

  return atom != pl_atom_false;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CHAR_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Char_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  atom = UnTag_ATM(word);
  if (tag_mask != TAG_ATM_MASK || pl_atom_tbl[atom].prop.length != 1)
    Pl_Err_Type(pl_type_character, word);

  return pl_atom_tbl[atom].name[0];
}




/*-------------------------------------------------------------------------*
 * PL_RD_CHAR                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Char(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);
  atom = UnTag_ATM(word);

  return pl_atom_tbl[atom].name[0];
}




/*-------------------------------------------------------------------------*
 * PL_RD_IN_CHAR_CHECK                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_In_Char_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  atom = UnTag_ATM(word);
  if (tag_mask != TAG_ATM_MASK ||
      (atom != pl_atom_end_of_file && pl_atom_tbl[atom].prop.length != 1))
    Pl_Err_Type(pl_type_in_character, word);

  return (atom != pl_atom_end_of_file) ? pl_atom_tbl[atom].name[0] : -1;
}




/*-------------------------------------------------------------------------*
 * PL_RD_IN_CHAR                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_In_Char(WamWord start_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(start_word, word, tag_mask);
  atom = UnTag_ATM(word);
  return (atom != pl_atom_end_of_file) ? pl_atom_tbl[atom].name[0] : -1;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CODE_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Code_Check(WamWord start_word)
{
  int c;

  c = Pl_Rd_Integer_Check(start_word);
  if (!Is_Valid_Code(c))
    Pl_Err_Representation(pl_representation_character_code);

  return c;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CODE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Code(WamWord start_word)
{
  return Pl_Rd_Integer(start_word);
}




/*-------------------------------------------------------------------------*
 * PL_RD_IN_CODE_CHECK                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_In_Code_Check(WamWord start_word)
{
  int c;

  c = Pl_Rd_Integer_Check(start_word);
  if (c != -1 && !Is_Valid_Code(c))
    Pl_Err_Representation(pl_representation_in_character_code);

  return c;
}




/*-------------------------------------------------------------------------*
 * PL_RD_IN_CODE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_In_Code(WamWord start_word)
{
  return Pl_Rd_Integer(start_word);
}




/*-------------------------------------------------------------------------*
 * PL_RD_BYTE_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Byte_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  PlLong c;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  c = UnTag_INT(word);
  if (tag_mask != TAG_INT_MASK || !Is_Valid_Byte(c))
    Pl_Err_Type(pl_type_byte, word);

  return c;
}




/*-------------------------------------------------------------------------*
 * PL_RD_BYTE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Byte(WamWord start_word)
{
  return Pl_Rd_Integer(start_word);
}




/*-------------------------------------------------------------------------*
 * PL_RD_IN_BYTE_CHECK                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_In_Byte_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  PlLong c;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  c = UnTag_INT(word);
  if (tag_mask != TAG_INT_MASK || (c != -1 && !Is_Valid_Byte(c)))
    Pl_Err_Type(pl_type_in_byte, word);

  return c;
}




/*-------------------------------------------------------------------------*
 * PL_RD_IN_BYTE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_In_Byte(WamWord start_word)
{
  return Pl_Rd_Integer(start_word);
}




/*-------------------------------------------------------------------------*
 * PL_RD_STRING_CHECK                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Rd_String_Check(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_ATM_MASK)
    Pl_Err_Type(pl_type_atom, word);

  return pl_atom_tbl[UnTag_ATM(word)].name;
}




/*-------------------------------------------------------------------------*
 * PL_RD_STRING                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Rd_String(WamWord start_word)
{
  return pl_atom_tbl[Pl_Rd_Atom(start_word)].name;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CHARS_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Rd_Chars_Check(WamWord start_word)
{
  Pl_Rd_Chars_Str_Check(start_word, pl_glob_buff);
  return pl_glob_buff;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CHARS                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Rd_Chars(WamWord start_word)
{
  Pl_Rd_Chars_Str(start_word, pl_glob_buff);
  return pl_glob_buff;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CODES_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Rd_Codes_Check(WamWord start_word)
{
  Pl_Rd_Codes_Str_Check(start_word, pl_glob_buff);
  return pl_glob_buff;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CODES                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Rd_Codes(WamWord start_word)
{
  Pl_Rd_Codes_Str(start_word, pl_glob_buff);
  return pl_glob_buff;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CHARS_STR_CHECK                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Chars_Str_Check(WamWord start_word, char *str)
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
	Pl_Err_Type(pl_type_list, save_start_word);

      lst_adr = UnTag_LST(word);

      *str++ = Pl_Rd_Char_Check(Car(lst_adr));
      n++;

      start_word = Cdr(lst_adr);
    }

  *str = '\0';
  return n;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CHARS_STR                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Chars_Str(WamWord start_word, char *str)
{
  WamWord word, tag_mask;
  WamWord *lst_adr;
  int n = 0;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (word == NIL_WORD)
	break;

      lst_adr = UnTag_LST(word);

      *str++ = Pl_Rd_Char_Check(Car(lst_adr));
      n++;

      start_word = Cdr(lst_adr);
    }

  *str = '\0';
  return n;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CODES_STR_CHECK                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Codes_Str_Check(WamWord start_word, char *str)
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
	Pl_Err_Type(pl_type_list, save_start_word);

      lst_adr = UnTag_LST(word);

      *str++ = Pl_Rd_Code_Check(Car(lst_adr));
      n++;

      start_word = Cdr(lst_adr);
    }

  *str = '\0';
  return n;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CODES_STR                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Codes_Str(WamWord start_word, char *str)
{
  WamWord word, tag_mask;
  WamWord *lst_adr;
  int n = 0;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (word == NIL_WORD)
	break;

      lst_adr = UnTag_LST(word);

      *str++ = Pl_Rd_Code_Check(Car(lst_adr));
      n++;

      start_word = Cdr(lst_adr);
    }

  *str = '\0';
  return n;
}




/*-------------------------------------------------------------------------*
 * PL_RD_LIST_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Rd_List_Check(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *lst_adr;

  DEREF(start_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (word == NIL_WORD)
    return NULL;

  if (tag_mask != TAG_LST_MASK)
    Pl_Err_Type(pl_type_list, start_word);

  lst_adr = UnTag_LST(word);
  return &Car(lst_adr);
}




/*-------------------------------------------------------------------------*
 * PL_RD_LIST                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Rd_List(WamWord start_word)
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
 * PL_RD_PROPER_LIST_CHECK                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Proper_List_Check(WamWord start_word, WamWord *arg)
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
	Pl_Err_Type(pl_type_list, save_start_word);

      lst_adr = UnTag_LST(word);

      *arg++ = Car(lst_adr);
      n++;

      start_word = Cdr(lst_adr);
    }

  return n;
}




/*-------------------------------------------------------------------------*
 * PL_RD_PROPER_LIST_CHECK2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int 
Pl_Rd_Proper_List_Check2(WamWord start_word, WamWord *arg, 
			 WamWord (*elt_fct)(WamWord start_word))

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
	Pl_Err_Type(pl_type_list, save_start_word);

      lst_adr = UnTag_LST(word);

      *arg++ = (*elt_fct)(Car(lst_adr));
      n++;

      start_word = Cdr(lst_adr);
    }

  return n;
}




/*-------------------------------------------------------------------------*
 * PL_RD_PROPER_LIST                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Rd_Proper_List(WamWord start_word, WamWord *arg)
{
  WamWord word, tag_mask;
  WamWord *lst_adr;
  int n = 0;

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
 * PL_RD_COMPOUND_CHECK                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Rd_Compound_Check(WamWord start_word, int *func, int *arity)
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

  Pl_Err_Type(pl_type_compound, start_word);
  return NULL;
}




/*-------------------------------------------------------------------------*
 * PL_RD_COMPOUND                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Rd_Compound(WamWord start_word, int *func, int *arity)
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
 * PL_RD_CALLABLE_CHECK                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Rd_Callable_Check(WamWord start_word, int *func, int *arity)
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

  Pl_Err_Type(pl_type_callable, start_word);
  return NULL;
}




/*-------------------------------------------------------------------------*
 * PL_RD_CALLABLE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Rd_Callable(WamWord start_word, int *func, int *arity)
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
 * PL_CHECK_FOR_UN_INTEGER                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Integer(WamWord start_word)
{
  CHECK_FOR_UN_INTEGER;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_POSITIVE                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Positive(WamWord start_word)
{
  CHECK_FOR_UN_POSITIVE;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_FLOAT                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Float(WamWord start_word)
{
  CHECK_FOR_UN_FLOAT;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_NUMBER                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Number(WamWord start_word)
{
  CHECK_FOR_UN_NUMBER;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_ATOM                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Atom(WamWord start_word)
{
  CHECK_FOR_UN_ATOM;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_BOOLEAN                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Boolean(WamWord start_word)
{
  CHECK_FOR_UN_BOOLEAN;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_CHAR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Char(WamWord start_word)
{
  CHECK_FOR_UN_CHAR;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_IN_CHAR                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_In_Char(WamWord start_word)
{
  CHECK_FOR_UN_IN_CHAR;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_CODE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Code(WamWord start_word)
{
  CHECK_FOR_UN_CODE;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_IN_CODE                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_In_Code(WamWord start_word)
{
  CHECK_FOR_UN_IN_CODE;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_BYTE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Byte(WamWord start_word)
{
  CHECK_FOR_UN_BYTE;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_IN_BYTE                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_In_Byte(WamWord start_word)
{
  CHECK_FOR_UN_IN_BYTE;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_PAIR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Pair(WamWord start_word)
{
  WamWord word, tag_mask;

static WamWord minus_2 = Functor_Arity(ATOM_CHAR('-'), 2);


  DEREF(start_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_STC_MASK || 
      Functor_And_Arity(UnTag_STC(word)) != minus_2))
    Pl_Err_Type(pl_type_pair, word);
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_CHARS                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Chars(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord save_start_word;
  WamWord *lst_adr;

  save_start_word = start_word;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK || word == NIL_WORD)
	return;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(pl_type_list, save_start_word);

      lst_adr = UnTag_LST(word);

      Pl_Check_For_Un_Char(Car(lst_adr));

      start_word = Cdr(lst_adr);
    }
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_STRING                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_String(WamWord start_word)
{
  Pl_Check_For_Un_Atom(start_word);
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_CODES                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Codes(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord save_start_word;
  WamWord *lst_adr;

  save_start_word = start_word;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK || word == NIL_WORD)
	return;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(pl_type_list, save_start_word);

      lst_adr = UnTag_LST(word);

      Pl_Check_For_Un_Code(Car(lst_adr));

      start_word = Cdr(lst_adr);
    }
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_LIST                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_List(WamWord start_word)
{
  if (!Pl_Blt_List_Or_Partial_List(start_word))
    Pl_Err_Type(pl_type_list, start_word);
}





/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_LIST2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_List2(WamWord start_word, void (*elt_fct)(WamWord start_word))
{
  WamWord start_word0 = start_word;
  WamWord word, tag_mask;
  WamWord *adr;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK || word == NIL_WORD)
	return;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(pl_type_list, start_word0);

      adr = UnTag_LST(word);

      (*elt_fct)(Car(adr));

      start_word = Cdr(adr);
    }
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_COMPOUND                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Compound(WamWord start_word)
{
  CHECK_FOR_UN_COMPOUND;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_CALLABLE                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Callable(WamWord start_word)
{
  CHECK_FOR_UN_CALLABLE;
}




/*-------------------------------------------------------------------------*
 * PL_CHECK_FOR_UN_VARIABLE                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Check_For_Un_Variable(WamWord start_word)
{
  CHECK_FOR_UN_VARIABLE;
}




/*-------------------------------------------------------------------------*
 * PL_UN_INTEGER_CHECK                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Integer_Check(PlLong value, WamWord start_word)
{
  CHECK_FOR_UN_INTEGER;

  return Pl_Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_INTEGER                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Integer(PlLong value, WamWord start_word)
{
  return Pl_Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_POSITIVE_CHECK                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Positive_Check(PlLong value, WamWord start_word)
{
  CHECK_FOR_UN_POSITIVE;

  return Pl_Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_POSITIVE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Positive(PlLong value, WamWord start_word)
{
  return Pl_Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_FLOAT_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Float_Check(double value, WamWord start_word)
{
  CHECK_FOR_UN_FLOAT;

  return Pl_Get_Float(value, word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_FLOAT                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Float(double value, WamWord start_word)
{
  return Pl_Get_Float(value, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_NUMBER_CHECK                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Number_Check(double value, WamWord start_word)
{
  PlLong n;

  CHECK_FOR_UN_NUMBER;

  n = (PlLong) value;

  return (n == value) ? Pl_Get_Integer(n, word) : Pl_Get_Float(value, word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_NUMBER                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Number(double value, WamWord start_word)
{
  PlLong n;

  n = (PlLong) value;

  return (n == value) ? Pl_Get_Integer(n, start_word) :
    Pl_Get_Float(value, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_ATOM_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Atom_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_ATOM;

  return Pl_Get_Atom(value, word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_ATOM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Atom(int value, WamWord start_word)
{
  return Pl_Get_Atom(value, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_BOOLEAN_CHECK                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Boolean_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_BOOLEAN;

  return Pl_Get_Atom(value ? pl_atom_true : pl_atom_false, word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_BOOLEAN                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Boolean(int value, WamWord start_word)
{
  return Pl_Get_Atom(value ? pl_atom_true : pl_atom_false, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_CHAR_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Char_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_CHAR;

  return Pl_Get_Atom(ATOM_CHAR(value), word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_CHAR                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Char(int value, WamWord start_word)
{
  return Pl_Get_Atom(ATOM_CHAR(value), start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_IN_CHAR_CHECK                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_In_Char_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_IN_CHAR;

  return Pl_Get_Atom((value == -1) ? pl_atom_end_of_file : ATOM_CHAR(value),
		  word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_IN_CHAR                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_In_Char(int value, WamWord start_word)
{
  return Pl_Get_Atom((value == -1) ? pl_atom_end_of_file : ATOM_CHAR(value),
		  start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_CODE_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Code_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_CODE;

  return Pl_Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_CODE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Code(int value, WamWord start_word)
{
  return Pl_Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_IN_CODE_CHECK                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_In_Code_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_IN_CODE;

  return Pl_Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_IN_CODE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_In_Code(int value, WamWord start_word)
{
  return Pl_Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_BYTE_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Byte_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_BYTE;

  return Pl_Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_BYTE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Byte(int value, WamWord start_word)
{
  return Pl_Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_IN_BYTE_CHECK                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_In_Byte_Check(int value, WamWord start_word)
{
  CHECK_FOR_UN_IN_BYTE;

  return Pl_Get_Integer(value, word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_IN_BYTE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_In_Byte(int value, WamWord start_word)
{
  return Pl_Get_Integer(value, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_STRING_CHECK                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_String_Check(char *value, WamWord start_word)
{
  CHECK_FOR_UN_ATOM;

  return Pl_Get_Atom(Pl_Create_Allocate_Atom(value), word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_STRING                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_String(char *value, WamWord start_word)
{
  return Pl_Get_Atom(Pl_Create_Allocate_Atom(value), start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_CHARS_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Chars_Check(char *str, WamWord start_word)
{
#if 0
  Pl_Check_For_Un_List(start_word);
#else
  Pl_Check_For_Un_Chars(start_word);
#endif

  return Pl_Un_Chars(str, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_CHARS                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Chars(char *str, WamWord start_word)
{
  for (; *str; str++)
    {
      if (!Pl_Get_List(start_word) || !Pl_Unify_Atom(ATOM_CHAR(*str)))
	return FALSE;

      start_word = Pl_Unify_Variable();
    }

  return Pl_Get_Nil(start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_CODES_CHECK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Codes_Check(char *str, WamWord start_word)
{
#if 0
  Pl_Check_For_Un_List(start_word);
#else
  Pl_Check_For_Un_Codes(start_word);
#endif

  return Pl_Un_Codes(str, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_CODES                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Codes(char *str, WamWord start_word)
{
  for (; *str; str++)
    {
      if (!Pl_Get_List(start_word) || !Pl_Unify_Integer(*str))
	return FALSE;

      start_word = Pl_Unify_Variable();
    }

  return Pl_Get_Nil(start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_LIST_CHECK                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_List_Check(WamWord *arg, WamWord start_word)
{
  WamWord word, tag_mask;

  if (arg == NULL)
    {
      if (Pl_Get_Nil(start_word))
	return TRUE;

    check_type:
      DEREF(start_word, word, tag_mask);
      if (word != NIL_WORD && tag_mask != TAG_LST_MASK)
	Pl_Err_Type(pl_type_list, start_word);

      return FALSE;
    }

  if (!Pl_Get_List(start_word))
    goto check_type;

  return Pl_Unify_Value(arg[0]) && Pl_Unify_Value(arg[1]);
}




/*-------------------------------------------------------------------------*
 * PL_UN_LIST                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_List(WamWord *arg, WamWord start_word)
{
  if (arg == NULL)
    return Pl_Get_Nil(start_word);

  return Pl_Get_List(start_word) && Pl_Unify_Value(arg[0]) && Pl_Unify_Value(arg[1]);
}




/*-------------------------------------------------------------------------*
 * PL_UN_PROPER_LIST_CHECK                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Proper_List_Check(int n, WamWord *arg, WamWord start_word)
{
  Pl_Check_For_Un_List(start_word);

  return Pl_Un_Proper_List(n, arg, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_PROPER_LIST                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Proper_List(int n, WamWord *arg, WamWord start_word)
{
  if (n < 0 || arg == NULL)
    n = 0;

  while (n--)
    {
      if (!Pl_Get_List(start_word) || !Pl_Unify_Value(*arg++))
	return FALSE;

      start_word = Pl_Unify_Variable();
    }

  return Pl_Get_Nil(start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_COMPOUND_CHECK                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Compound_Check(int func, int arity, WamWord *arg, WamWord start_word)
{
  int i;

  if (arity == 0)
    return Pl_Un_Atom_Check(func, start_word);

  if (arity == 2 && func == ATOM_CHAR('.'))
    return Pl_Un_List_Check(arg, start_word);

  if (!Pl_Get_Structure(func, arity, start_word))
    {
      if (!Pl_Blt_Compound(start_word))
	Pl_Err_Type(pl_type_compound, start_word);
      return FALSE;
    }

  for (i = 0; i < arity; i++)
    if (!Pl_Unify_Value(arg[i]))
      return FALSE;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_UN_COMPOUND                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Compound(int func, int arity, WamWord *arg, WamWord start_word)
{
  int i;

  if (arity == 0)
    return Pl_Un_Atom_Check(func, start_word);

  if (arity == 2 && func == ATOM_CHAR('.'))
    return Pl_Un_List(arg, start_word);

  if (!Pl_Get_Structure(func, arity, start_word))
    return FALSE;

  for (i = 0; i < arity; i++)
    if (!Pl_Unify_Value(arg[i]))
      return FALSE;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_UN_CALLABLE_CHECK                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Callable_Check(int func, int arity, WamWord *arg, WamWord start_word)
{
  int i;

  if (arity == 0)
    return Pl_Un_Atom_Check(func, start_word);

  if (arity == 2 && func == ATOM_CHAR('.'))
    return Pl_Un_List_Check(arg, start_word);

  if (!Pl_Get_Structure(func, arity, start_word))
    {
      if (!Pl_Blt_Callable(start_word))
	Pl_Err_Type(pl_type_callable, start_word);
      return FALSE;
    }

  for (i = 0; i < arity; i++)
    if (!Pl_Unify_Value(arg[i]))
      return FALSE;

  return TRUE;
}



/*-------------------------------------------------------------------------*
 * PL_UN_CALLABLE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Callable(int func, int arity, WamWord *arg, WamWord start_word)
{
  return Pl_Un_Compound(func, arity, arg, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_UN_TERM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Un_Term(WamWord term_word, WamWord start_word)
{				/* used because Unify is FC convention */
  return Pl_Unify(term_word, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_MK_INTEGER                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Integer(PlLong value)
{
  return Pl_Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * PL_MK_POSITIVE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Positive(PlLong value)
{
  return Pl_Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * PL_MK_FLOAT                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Float(double value)
{
  return Pl_Put_Float(value);
}




/*-------------------------------------------------------------------------*
 * PL_MK_NUMBER                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Number(double value)
{
  int n;

  n = (PlLong) value;

  if (n == value)
    return Pl_Put_Integer(n);

  return Pl_Put_Float(value);
}




/*-------------------------------------------------------------------------*
 * PL_MK_ATOM                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Atom(int value)
{
  return Pl_Put_Atom(value);
}




/*-------------------------------------------------------------------------*
 * PL_MK_BOOLEAN                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Boolean(int value)
{
  return Pl_Put_Atom(value ? pl_atom_true : pl_atom_false);
}




/*-------------------------------------------------------------------------*
 * PL_MK_CHAR                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Char(int value)
{
  return Pl_Put_Atom(ATOM_CHAR(value));
}




/*-------------------------------------------------------------------------*
 * PL_MK_IN_CHAR                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_In_Char(int value)
{
  return Pl_Put_Atom((value == -1) ? pl_atom_end_of_file : ATOM_CHAR(value));
}




/*-------------------------------------------------------------------------*
 * PL_MK_CODE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Code(int value)
{
  return Pl_Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * PL_MK_IN_CODE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_In_Code(int value)
{
  return Pl_Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * PL_MK_BYTE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Byte(int value)
{
  return Pl_Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * PL_MK_IN_BYTE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_In_Byte(int value)
{
  return Pl_Put_Integer(value);
}




/*-------------------------------------------------------------------------*
 * PL_MK_STRING                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_String(char *value)
{
  return Pl_Put_Atom(Pl_Create_Allocate_Atom(value));
}




/*-------------------------------------------------------------------------*
 * PL_MK_CHARS                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Chars(char *str)
{
  WamWord res_word;

  if (*str == '\0')
    return NIL_WORD;

  res_word = Pl_Put_List();
  for (;;)
    {
      Pl_Unify_Atom(ATOM_CHAR(*str));
      str++;
      if (*str == '\0')
	break;

      Pl_Unify_List();
    }

  Pl_Unify_Nil();
  return res_word;
}




/*-------------------------------------------------------------------------*
 * PL_MK_CODES                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Codes(char *str)
{
  WamWord res_word;

  if (*str == '\0')
    return NIL_WORD;

  res_word = Pl_Put_List();
  for (;;)
    {
      Pl_Unify_Integer(*str);
      str++;
      if (*str == '\0')
	break;

      Pl_Unify_List();
    }

  Pl_Unify_Nil();
  return res_word;
}




/*-------------------------------------------------------------------------*
 * PL_MK_LIST                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_List(WamWord *arg)
{
  WamWord res_word;

  if (arg == NULL)
    return NIL_WORD;

  res_word = Pl_Put_List();
  Pl_Unify_Value(arg[0]);
  Pl_Unify_Value(arg[1]);

  return res_word;
}








/*-------------------------------------------------------------------------*
 * PL_MK_PROPER_LIST                                                       *
 *                                                                         *
 * This function transform an array of n WamWords located at arg into a    *
 * Prolog list (pushed at the top of the heap) and returns the resulting   *
 * list word.                                                              *
 * Note: arg can be equal to H to tranform an array into a list in-place.  *
 * The resulting list uses 2*n WamWord from the top of the heap.           *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Proper_List(int n, WamWord *arg)
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
 * PL_MK_COMPOUND                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Compound(int func, int arity, WamWord *arg)
{
  WamWord res_word;
  int i;

  if (arity == 0)
    return Pl_Put_Atom(func);

  if (arity == 2 && func == ATOM_CHAR('.'))
    return Pl_Mk_List(arg);

  res_word = Pl_Put_Structure(func, arity);
  for (i = 0; i < arity; i++)
    Pl_Unify_Value(arg[i]);

  return res_word;
}




/*-------------------------------------------------------------------------*
 * PL_MK_CALLABLE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Callable(int func, int arity, WamWord *arg)
{
  return Pl_Mk_Compound(func, arity, arg);
}




/*-------------------------------------------------------------------------*
 * PL_MK_VARIABLE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Mk_Variable(void)
{
  return Pl_Put_X_Variable();
}
