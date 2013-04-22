/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : atom_c.c                                                        *
 * Descr.: atom manipulation management - C part                           *
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


#include <stdlib.h>
#include <string.h>
#include <ctype.h>

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

static Bool
Compute_Next_BLA(int mask, AtomInf *patom, AtomInf *psub_atom,
		 int b, int l, int a, int *b1, int *l1, int *a1);



static int Create_Malloc_Atom(char *str);

static Bool String_To_Number(char *str, WamWord number_word);



#define ATOM_CONCAT_ALT            X1_2461746F6D5F636F6E6361745F616C74

#define SUB_ATOM_ALT               X1_247375625F61746F6D5F616C74

#define CURRENT_ATOM_ALT           X1_2463757272656E745F61746F6D5F616C74



Prolog_Prototype(ATOM_CONCAT_ALT, 0);
Prolog_Prototype(SUB_ATOM_ALT, 0);
Prolog_Prototype(CURRENT_ATOM_ALT, 0);

#define MALLOC_STR(n)				\
   if (n<0)					\
     return FALSE;				\
   str = (char *) Malloc(n + 1)




/*-------------------------------------------------------------------------*
 * PL_ATOM_LENGTH_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Atom_Length_2(WamWord atom_word, WamWord length_word)
{
  int atom;

  atom = Pl_Rd_Atom_Check(atom_word);
  return Pl_Un_Positive_Check(pl_atom_tbl[atom].prop.length, length_word);
}




/*-------------------------------------------------------------------------*
 * PL_NEW_ATOM_2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_New_Atom_2(WamWord prefix_word, WamWord atom_word)
{
  int atom;

  atom = Pl_Rd_Atom_Check(prefix_word);
  Pl_Check_For_Un_Variable(atom_word);

  return Pl_Get_Atom(Pl_Gen_New_Atom(pl_atom_tbl[atom].name), atom_word);
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_CONCAT_3                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Atom_Concat_3(WamWord atom1_word, WamWord atom2_word, WamWord atom3_word)
{
  WamWord word, tag_mask;
  int tag1, tag2, tag3;
  AtomInf *patom1, *patom2, *patom3;
  char *str;
  int l;


  DEREF(atom1_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK)
    Pl_Err_Type(pl_type_atom, atom1_word);
  tag1 = tag_mask;
  atom1_word = word;


  DEREF(atom2_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK)
    Pl_Err_Type(pl_type_atom, atom2_word);
  tag2 = tag_mask;
  atom2_word = word;


  DEREF(atom3_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK)
    Pl_Err_Type(pl_type_atom, atom3_word);
  tag3 = tag_mask;
  atom3_word = word;


  if (tag3 == TAG_REF_MASK && (tag1 == TAG_REF_MASK || tag2 == TAG_REF_MASK))
    Pl_Err_Instantiation();


  if (tag1 == TAG_ATM_MASK)
    {
      patom1 = pl_atom_tbl + UnTag_ATM(atom1_word);

      if (tag2 == TAG_ATM_MASK)
	{
	  patom2 = pl_atom_tbl + UnTag_ATM(atom2_word);
	  l = patom1->prop.length + patom2->prop.length;
	  MALLOC_STR(l);
	  strcpy(str, patom1->name);
	  strcpy(str + patom1->prop.length, patom2->name);
	  return Pl_Get_Atom(Create_Malloc_Atom(str), atom3_word);
	}

      patom3 = pl_atom_tbl + UnTag_ATM(atom3_word);
      l = patom3->prop.length - patom1->prop.length;
      if (l < 0 || strncmp(patom1->name, patom3->name, patom1->prop.length) != 0)
	return FALSE;
      MALLOC_STR(l);
      strcpy(str, patom3->name + patom1->prop.length);

      return Pl_Get_Atom(Create_Malloc_Atom(str), atom2_word);
    }

  if (tag2 == TAG_ATM_MASK)	/* here tag1 == REF */
    {
      patom2 = pl_atom_tbl + UnTag_ATM(atom2_word);
      patom3 = pl_atom_tbl + UnTag_ATM(atom3_word);
      l = patom3->prop.length - patom2->prop.length;
      if (l < 0 || strncmp(patom2->name, patom3->name + l, patom2->prop.length) != 0)
	return FALSE;

      MALLOC_STR(l);
      strncpy(str, patom3->name, l);
      str[l] = '\0';

      return Pl_Get_Atom(Create_Malloc_Atom(str), atom1_word);
    }

  /* A1 and A2 are variables: non deterministic case */

  patom3 = pl_atom_tbl + UnTag_ATM(atom3_word);

  if (patom3->prop.length > 0)
    {
      A(0) = atom1_word;
      A(1) = atom2_word;
      A(2) = (WamWord) patom3;
      A(3) = (WamWord) (patom3->name + 1);
      Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(ATOM_CONCAT_ALT, 0), 4);
    }

  return Pl_Get_Atom(pl_atom_void, atom1_word) &&
    Pl_Get_Atom_Tagged(atom3_word, atom2_word);
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_CONCAT_ALT_0                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Atom_Concat_Alt_0(void)
{
  WamWord atom1_word, atom2_word;
  AtomInf *patom3;
  char *name;
  char *p;
  char *str;
  int l;

  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(ATOM_CONCAT_ALT, 0), 0);

  atom1_word = AB(B, 0);
  atom2_word = AB(B, 1);
  patom3 = (AtomInf *) AB(B, 2);
  p = (char *) AB(B, 3);

  if (*p == '\0')
    Delete_Last_Choice_Point();
  else				/* non deterministic case */
    {
#if 0 /* the following data is unchanged */
      AB(B, 0) = atom1_word;
      AB(B, 1) = atom2_word;
      AB(B, 2) = (WamWord) patom3;
#endif
      AB(B, 3) = (WamWord) (p + 1);
    }

  name = patom3->name;

  l = p - name;
  MALLOC_STR(l);
  strncpy(str, name, l + 1);
  str[l] = '\0';
  if (!Pl_Get_Atom(Create_Malloc_Atom(str), atom1_word))
    return FALSE;

  l = patom3->prop.length - l;
  MALLOC_STR(l);
  strcpy(str, p);
  return Pl_Get_Atom(Create_Malloc_Atom(str), atom2_word);
}




#define DEREF_LG(lg_word, lg)                                               \
  DEREF(lg_word, word, tag_mask);                                           \
  mask <<= 1;                                                               \
  if (tag_mask == TAG_INT_MASK)                                             \
    {                                                                       \
      if ((lg = UnTag_INT(word)) < 0)                                       \
        Pl_Err_Domain(pl_domain_not_less_than_zero, word);                  \
      mask |= 1;                                                            \
    }                                                                       \
  else                                                                      \
    {                                                                       \
      lg = 0;                                                               \
      if (tag_mask != TAG_REF_MASK)                                         \
        Pl_Err_Type(pl_type_integer, word);                                 \
    }                                                                       \
  lg_word = word




/*-------------------------------------------------------------------------*
 * PL_SUB_ATOM_5                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Sub_Atom_5(WamWord atom_word, WamWord before_word, WamWord length_word,
	   WamWord after_word, WamWord sub_atom_word)
{
  WamWord word, tag_mask;
  AtomInf *patom;
  AtomInf *psub_atom = NULL;	/* only for the compiler */
  int length;
  PlLong b, l, a;
  int b1, l1, a1;
  Bool nondet;
  int mask = 0;
  char *str;

  patom = pl_atom_tbl + Pl_Rd_Atom_Check(atom_word);
  length = patom->prop.length;


  DEREF_LG(before_word, b);
  DEREF_LG(length_word, l);
  DEREF_LG(after_word, a);


  DEREF(sub_atom_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK)
    Pl_Err_Type(pl_type_atom, word);
  sub_atom_word = word;
  if (tag_mask == TAG_ATM_MASK)
    {
      psub_atom = pl_atom_tbl + UnTag_ATM(word);
      l = psub_atom->prop.length;
      if (!Pl_Get_Integer(l, length_word))
	return FALSE;

      if ((mask & 5) == 5 && length != b + l + a) /* B and A fixed */
	return FALSE;

      if (mask & 4)		/* B fixed */
	{
	  a = length - b - l;
	  return strncmp(patom->name + b, psub_atom->name, l) == 0 &&
	    Pl_Get_Integer(a, after_word);
	}

      if (mask & 1)		/* A fixed */
	{
	  b = length - l - a;
	  return strncmp(patom->name + b, psub_atom->name, l) == 0 &&
	    Pl_Get_Integer(b, before_word);
	}
      mask = 8;			/* set sub_atom as fixed */
    }


  switch (mask)			/* mask <= 7, B L A (1: fixed, 0: var) */
    {
    case 0:			/* nothing fixed */
    case 2:			/* L fixed */
    case 4:			/* B fixed */
      a = length - b - l;
      nondet = TRUE;
      break;

    case 1:			/* A fixed */
      l = length - b - a;
      nondet = TRUE;
      break;

    case 3:			/* L A fixed */
      b = length - l - a;
      nondet = FALSE;
      break;

    case 5:			/* B A fixed */
      l = length - b - a;
      nondet = FALSE;
      break;

    case 6:			/* B L fixed */
    case 7:			/* B L A fixed */
      a = length - b - l;
      nondet = FALSE;
      break;

    default:			/* sub_atom fixed */
      if ((str = strstr(patom->name + b, psub_atom->name)) == NULL)
	return FALSE;

      b = str - patom->name;
      a = length - b - l;
      nondet = TRUE;
      break;
    }

  if (b < 0 || l < 0 || a < 0)
    return FALSE;

  if (nondet
      && Compute_Next_BLA(mask, patom, psub_atom, b, l, a, &b1, &l1, &a1))
    {				/* non deterministic case */
      A(0) = before_word;
      A(1) = length_word;
      A(2) = after_word;
      A(3) = sub_atom_word;
      A(4) = (WamWord) patom;
      A(5) = (WamWord) psub_atom;
      A(6) = mask;
      A(7) = b1;
      A(8) = l1;
      A(9) = a1;

      Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(SUB_ATOM_ALT, 0), 10);
    }

  if (mask <= 7)
    {
      MALLOC_STR(l);
      strncpy(str, patom->name + b, l);
      str[l] = '\0';
      Pl_Get_Atom(Create_Malloc_Atom(str), sub_atom_word);
      Pl_Get_Integer(l, length_word);
    }

  return Pl_Get_Integer(b, before_word) && Pl_Get_Integer(a, after_word);
}




/*-------------------------------------------------------------------------*
 * PL_SUB_ATOM_ALT_0                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Sub_Atom_Alt_0(void)
{
  WamWord before_word, length_word, after_word, sub_atom_word;
  AtomInf *patom;
  AtomInf *psub_atom;
  int b, l, a;
  int b1, l1, a1;
  int mask;
  char *str;

  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(SUB_ATOM_ALT, 0), 0);

  before_word = AB(B, 0);
  length_word = AB(B, 1);
  after_word = AB(B, 2);
  sub_atom_word = AB(B, 3);
  patom = (AtomInf *) AB(B, 4);
  psub_atom = (AtomInf *) AB(B, 5);
  mask = AB(B, 6);
  b = AB(B, 7);
  l = AB(B, 8);
  a = AB(B, 9);


  if (!Compute_Next_BLA(mask, patom, psub_atom, b, l, a, &b1, &l1, &a1))
    Delete_Last_Choice_Point();
  else				/* non deterministic case */
    {
#if 0 /* the following data is unchanged */
      AB(B, 0) = before_word;
      AB(B, 1) = length_word;
      AB(B, 2) = after_word;
      AB(B, 3) = sub_atom_word;
      AB(B, 4) = (WamWord) patom;
      AB(B, 5) = (WamWord) psub_atom;
      AB(B, 6) = mask;
#endif
      AB(B, 7) = b1;
      AB(B, 8) = l1;
      AB(B, 9) = a1;
    }

  if (mask <= 7)
    {
      MALLOC_STR(l);
      strncpy(str, patom->name + b, l);
      str[l] = '\0';
      Pl_Get_Atom(Create_Malloc_Atom(str), sub_atom_word);
      Pl_Get_Integer(l, length_word);
    }

  return Pl_Get_Integer(b, before_word) && Pl_Get_Integer(a, after_word);
}




/*-------------------------------------------------------------------------*
 * COMPUTE_NEXT_BLA                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Compute_Next_BLA(int mask, AtomInf *patom, AtomInf *psub_atom,
		 int b, int l, int a, int *b1, int *l1, int *a1)
{
  int length = patom->prop.length;
  char *str;


  switch (mask)			/* mask B L A (1: fixed, 0: var) */
    {
    case 0:			/* nothing fixed */
      if (++l > length - b)
	{
	  l = 0;
	  if (++b > length)
	    return FALSE;
	}
      a = length - b - l;
      break;

    case 1:			/* A fixed */
      if (++b > length - a)
	return FALSE;
      l = length - b - a;
      break;

    case 2:			/* L fixed */
      if (++b > length - l)
	return FALSE;
      a = length - b - l;
      break;

    case 4:			/* B fixed */
      if (++l > length - b)
	return FALSE;
      a = length - b - l;
      break;

    default:			/* sub_atom fixed */
      if (++b > length - l)
	return FALSE;

      if ((str = strstr(patom->name + b, psub_atom->name)) == NULL)
	return FALSE;

      b = str - patom->name;
      a = length - b - l;
      break;
    }

  *b1 = b;
  *l1 = l;
  *a1 = a;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * CREATE_MALLOC_ATOM                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Create_Malloc_Atom(char *str)
{
  int atom;
  int nb = pl_nb_atom;

  atom = Pl_Create_Atom(str);
  if (nb == pl_nb_atom)
    Free(str);
  return atom;
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_CHARS_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Atom_Chars_2(WamWord atom_word, WamWord chars_word)
{
  WamWord word, tag_mask;

  DEREF(atom_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    return Pl_Un_Chars_Check(Pl_Rd_String_Check(word), chars_word);

  return Pl_Un_String_Check(Pl_Rd_Chars_Check(chars_word), atom_word);
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_CODES_2                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Atom_Codes_2(WamWord atom_word, WamWord codes_word)
{
  WamWord word, tag_mask;

  DEREF(atom_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    return Pl_Un_Codes_Check(Pl_Rd_String_Check(word), codes_word);

  return Pl_Un_String_Check(Pl_Rd_Codes_Check(codes_word), atom_word);
}




/*-------------------------------------------------------------------------*
 * PL_NUMBER_ATOM_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Number_Atom_2(WamWord number_word, WamWord atom_word)
{
  WamWord word, tag_mask;
  char *str;

  DEREF(atom_word, word, tag_mask);
  if (tag_mask == TAG_ATM_MASK)
    return String_To_Number(pl_atom_tbl[UnTag_ATM(word)].name, number_word);

  if (tag_mask != TAG_REF_MASK)
    Pl_Err_Type(pl_type_atom, word);

  DEREF(number_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    {
      sprintf(pl_glob_buff, "%" PL_FMT_d, UnTag_INT(word));
      return Pl_Un_String_Check(pl_glob_buff, atom_word);
    }

  str = Pl_Float_To_String(Pl_Rd_Number_Check(word));
  return Pl_Un_String_Check(str, atom_word);
}




/*-------------------------------------------------------------------------*
 * PL_NUMBER_CHARS_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Number_Chars_2(WamWord number_word, WamWord chars_word)
{
  WamWord word, tag_mask;
  WamWord *lst_adr, list_word;
  char *str = pl_glob_buff;
  int atom;

  list_word = chars_word;
  for (;;)
    {
      DEREF(list_word, word, tag_mask);

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	goto from_nb;

      lst_adr = UnTag_LST(word);
      DEREF(Car(lst_adr), word, tag_mask);
      atom = UnTag_ATM(word);
      if (tag_mask != TAG_ATM_MASK || pl_atom_tbl[atom].prop.length != 1)
	goto from_nb;

      *str++ = pl_atom_tbl[atom].name[0];
      list_word = Cdr(lst_adr);
    }

  *str = '\0';
  return String_To_Number(pl_glob_buff, number_word);

from_nb:
  DEREF(number_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    {
      sprintf(pl_glob_buff, "%" PL_FMT_d, UnTag_INT(word));
      return Pl_Un_Chars_Check(pl_glob_buff, chars_word);
    }

  if (tag_mask != TAG_REF_MASK)
    {
      str = Pl_Float_To_String(Pl_Rd_Number_Check(word));
      return Pl_Un_Chars_Check(str, chars_word);
    }

  Pl_Rd_Chars_Check(chars_word);	/* only to raise the correct error */
  return FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_NUMBER_CODES_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Number_Codes_2(WamWord number_word, WamWord codes_word)
{
  WamWord word, tag_mask;
  WamWord *lst_adr, list_word;
  char *str = pl_glob_buff;
  PlLong c;

  list_word = codes_word;
  for (;;)
    {
      DEREF(list_word, word, tag_mask);

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	goto from_nb;

      lst_adr = UnTag_LST(word);
      DEREF(Car(lst_adr), word, tag_mask);
      c = UnTag_INT(word);
      if (tag_mask != TAG_INT_MASK || !Is_Valid_Code(c))
	goto from_nb;

      *str++ = (char) c;
      list_word = Cdr(lst_adr);
    }

  *str = '\0';
  return String_To_Number(pl_glob_buff, number_word);

from_nb:
  DEREF(number_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    {
      sprintf(pl_glob_buff, "%" PL_FMT_d, UnTag_INT(word));
      return Pl_Un_Codes_Check(pl_glob_buff, codes_word);
    }

  if (tag_mask != TAG_REF_MASK)
    {
      str = Pl_Float_To_String(Pl_Rd_Number_Check(word));
      return Pl_Un_Codes_Check(str, codes_word);
    }

  Pl_Rd_Codes_Check(codes_word);	/* only to raise the correct error */
  return FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_CHAR_CODE_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Char_Code_2(WamWord char_word, WamWord code_word)
{
  WamWord word, tag_mask;

  DEREF(char_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    return Pl_Un_Code_Check(Pl_Rd_Char_Check(word), code_word);

  return Pl_Un_Char_Check(Pl_Rd_Code_Check(code_word), char_word);
}




/*-------------------------------------------------------------------------*
 * PL_NAME_2                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Name_2(WamWord atomic_word, WamWord codes_word)
{
  WamWord word, tag_mask;
  int syn_flag;
  Bool is_number;
  char *str;


  DEREF(atomic_word, word, tag_mask);
  if (tag_mask == TAG_ATM_MASK)
    return Pl_Atom_Codes_2(word, codes_word);

  if (tag_mask == TAG_INT_MASK || tag_mask == TAG_FLT_MASK)
    return Pl_Number_Codes_2(word, codes_word);

  if (tag_mask != TAG_REF_MASK)
    Pl_Err_Type(pl_type_atomic, word);


  str = Pl_Rd_Codes_Check(codes_word);

  syn_flag = Flag_Value(syntax_error);
  Flag_Value(syntax_error) = PF_ERR_FAIL;

  is_number = String_To_Number(str, word);	/* only fails on syn err */

  Flag_Value(syntax_error) = syn_flag;

  if (is_number)
    return TRUE;

  return Pl_Un_String(str, word);
}




/*-------------------------------------------------------------------------*
 * PL_LOWER_UPPER_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Lower_Upper_2(WamWord lower_word, WamWord upper_word)
{
  WamWord word, tag_mask;

  DEREF(lower_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    return Pl_Un_Char_Check(toupper(Pl_Rd_Char_Check(word)), upper_word);

  return Pl_Un_Char_Check(tolower(Pl_Rd_Char_Check(upper_word)), lower_word);
}




/*-------------------------------------------------------------------------*
 * STRING_TO_NUMBER                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
String_To_Number(char *str, WamWord number_word)
{
  WamWord word;
  int stm;
  StmInf *pstm;
  Bool eof;

  Pl_Check_For_Un_Number(number_word);

/* #if 0 since layout leading chars allowed in ISO cf. number_chars */
#if 0
  if (!isdigit(*str) && *str != '-')
    {
      Pl_Set_Last_Syntax_Error("", 1, 1, "non numeric character");
      goto err;
    }
#endif

  stm = Pl_Add_Str_Stream(str, TERM_STREAM_ATOM);
  pstm = pl_stm_tbl[stm];

  word = Pl_Read_Number(pstm);
  eof = (Pl_Stream_Peekc(pstm) == EOF);

  if (word != NOT_A_WAM_WORD && !eof)
    Pl_Set_Last_Syntax_Error(pl_atom_tbl[pstm->atom_file_name].name,
			  pstm->line_count + 1, pstm->line_pos + 1,
			  "non numeric character");

  Pl_Delete_Str_Stream(stm);

  if (word == NOT_A_WAM_WORD || !eof)
    {
#if 0
    err:
#endif
      Pl_Syntax_Error(Flag_Value(syntax_error));
      return FALSE;
    }

  return Pl_Unify(word, number_word);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_ATOM_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Atom_2(WamWord atom_word, WamWord hide_word)
{
  WamWord word, tag_mask;
  Bool hide;
  int atom;

  hide = Pl_Rd_Integer_Check(hide_word);

  DEREF(atom_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    return *Pl_Rd_String_Check(word) != '$' || !hide;

  atom = -1;
  for (;;)
    {
      atom = Pl_Find_Next_Atom(atom);
      if (atom == -1)
	return FALSE;

      if (!hide || pl_atom_tbl[atom].name[0] != '$')
	break;
    }
				/* non deterministic case */
  A(0) = atom_word;
  A(1) = hide;
  A(2) = atom;
  Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_ATOM_ALT, 0), 3);

  return Pl_Get_Atom(atom, atom_word);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_ATOM_ALT_0                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Atom_Alt_0(void)
{
  WamWord atom_word;
  Bool hide;
  int atom;

  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_ATOM_ALT, 0), 0);

  atom_word = AB(B, 0);
  hide = AB(B, 1);
  atom = AB(B, 2);

  for (;;)
    {
      atom = Pl_Find_Next_Atom(atom);
      if (atom == -1)
	{
	  Delete_Last_Choice_Point();
	  return FALSE;
	}

      if (!hide || pl_atom_tbl[atom].name[0] != '$')
	break;
    }
				/* non deterministic case */

#if 0				/* the following data is unchanged */
  AB(B, 0) = atom_word;
  AB(B, 1) = hide;
#endif
  AB(B, 2) = atom;

  return Pl_Get_Atom(atom, atom_word);
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_PROPERTY_6                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Atom_Property_6(WamWord atom_word,
		   WamWord prefix_op_word, WamWord infix_op_word,
		   WamWord postfix_op_word,
		   WamWord needs_quote_word, WamWord needs_scan_word)
{
  WamWord word, tag_mask;
  int atom;

  DEREF(atom_word, word, tag_mask);
  atom = UnTag_ATM(word);

  Pl_Get_Integer(Check_Oper(atom, PREFIX) != 0, prefix_op_word);
  Pl_Get_Integer(Check_Oper(atom, INFIX) != 0, infix_op_word);
  Pl_Get_Integer(Check_Oper(atom, POSTFIX) != 0, postfix_op_word);

  Pl_Get_Integer(pl_atom_tbl[atom].prop.needs_quote, needs_quote_word);
  Pl_Get_Integer(pl_atom_tbl[atom].prop.needs_scan, needs_scan_word);
}
