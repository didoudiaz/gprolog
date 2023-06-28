/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : flag_supp.c                                                     *
 * Descr.: Prolog flag and system variable support                         *
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


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "engine_pl.h"
#include "bips_pl.h"


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define NB_OF_FLAGS               64

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static FlagInf flag_tbl[NB_OF_FLAGS];
static int nb_flag;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static FlagInf *Prolog_Flag_Lookup(int atom_name);

#define CURRENT_PROLOG_FLAG_ALT    X1_2463757272656E745F70726F6C6F675F666C61675F616C74

Prolog_Prototype(CURRENT_PROLOG_FLAG_ALT, 0);


/*-------------------------------------------------------------------------*
 * Prolog flag support                                                     *
 *                                                                         *
 * Since 1.4.4 Prolog flags are handled in a more general way in order to  *
 * simplify the addition of new flags.                                     *
 * Since 1.6.0 it has been further generalized with the introduction of    *
 * flags whose value is defined by a set (table) of possible atoms.        *
 * The associated value is the index in the table.                         *
 * This simplified many types now reduced as a table of atoms: false/true, *
 * on/off, error/warning/fail, chars/codes/atom/...                        *
 *                                                                         *
 * A flag has the following attributes (see flag_supp.h) :                 *
 *   - a name (string -> atom)                                             *
 *   - is modifiable or not (ro/rw)                                        *
 *   - a type (3 predefined types + 'any' for customization)               *
 *   - a value (a PlLong) used for predefined types and available for 'any'*
 *   - a function get: used by current_prolog_flag                         *
 *   - a function chk: used by set_prolog_flag (even if not modifiable)    *
 *   - a function set: used by set_prolog_flag (only if modifiable)        *
 *                                                                         *
 * The function chk is used by set_prolog_flag to check if the value is    *
 * compatible with the flag even if the flag is read-only. If the function *
 * fails a domain_error is raised (if it succeeds and the flag is read-only*
 * the a permission_error will be raised).                                 *
 * This function is not used by current_prolog_flag (as requiered in ISO). *
 *                                                                         *
 * For some flags it is not clear what to check, e.g. for version_data we  *
 * currently check the value is a struct gprolog/4 (args are not checked). *
 * This function can be omitted (in that case no domain_error is checked). *
 *-------------------------------------------------------------------------*/


/*-------------------------------------------------------------------------*
 * FCT_GET_xxx FCT_CHK_xxx FCT_SET_xxx                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Fct_Get_Integer(FlagInf *flag)
{
  return Tag_INT(flag->value);
}

static Bool
Fct_Chk_Integer(FlagInf *flag, WamWord tag_mask, WamWord value_word)
{
  return tag_mask == TAG_INT_MASK;
}

static Bool
Fct_Set_Integer(FlagInf *flag, WamWord value_word)
{
  flag->value = UnTag_INT(value_word);
  return TRUE;
}




static WamWord
Fct_Get_Atom(FlagInf *flag)
{
  return Tag_ATM(flag->value);
}

static Bool
Fct_Chk_Atom(FlagInf *flag, WamWord tag_mask, WamWord value_word)
{
  return tag_mask == TAG_ATM_MASK;
}

static Bool
Fct_Set_Atom(FlagInf *flag, WamWord value_word)
{
  flag->value = UnTag_ATM(value_word);
  return TRUE;
}




static WamWord
Fct_Get_Atom_Tbl(FlagInf *flag)
{
  return Tag_ATM(flag->tbl_atom[flag->value]);
}

static Bool
Fct_Chk_Atom_Tbl(FlagInf *flag, WamWord tag_mask, WamWord value_word)
{
  int atom;
  int i;

  if (tag_mask != TAG_ATM_MASK)
    return FALSE;

  atom = UnTag_ATM(value_word);
  for(i = 0; flag->tbl_atom[i] >= 0 && flag->tbl_atom[i] != atom; i++)
    ;

  return (flag->tbl_atom[i] == atom);
}

static Bool
Fct_Set_Atom_Tbl(FlagInf *flag, WamWord value_word)
{
  int atom;
  int i;

  atom = UnTag_ATM(value_word);
  for(i = 0; flag->tbl_atom[i] != atom; i++)
    ;

  flag->value = i;
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_NEW_PROLOG_FLAG                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
FlagInf *
Pl_New_Prolog_Flag(char *name, Bool modifiable, FlagType type, PlLong value,
		   FlagFctGet fct_get, FlagFctChk fct_chk, FlagFctSet fct_set, ...)
{
  int atom_name;
  FlagInf *flag;
  
  atom_name = Pl_Create_Atom(name);

  if (nb_flag == NB_OF_FLAGS)
    Pl_Fatal_Error("Flag table full - increase NB_OF_FLAGS = %d", NB_OF_FLAGS);

  flag = flag_tbl + nb_flag++;


  flag->atom_name = atom_name;
  flag->modifiable = modifiable;
  flag->type = type;
  flag->value = value;

  flag->fct_get = fct_get;
  flag->fct_chk = fct_chk;
  flag->fct_set = fct_set;

  if (fct_get == NULL)
    {
      switch(type)
	{
	case PF_TYPE_INTEGER:
	  flag->fct_get = Fct_Get_Integer;
	  break;

	case PF_TYPE_ATOM:
	  flag->fct_get = Fct_Get_Atom;
	  break;

	case PF_TYPE_ATOM_TBL:
	  flag->fct_get = Fct_Get_Atom_Tbl;
	  break;

	case PF_TYPE_ANY:
	  break;	/* should not occur */
	}
    }

  if (fct_chk == NULL)
    {
      switch(type)
	{
	case PF_TYPE_INTEGER:
	  flag->fct_chk = Fct_Chk_Integer;
	  break;

	case PF_TYPE_ATOM:
	  flag->fct_chk = Fct_Chk_Atom;
	  break;

	case PF_TYPE_ATOM_TBL:
	  flag->fct_chk = Fct_Chk_Atom_Tbl;
	  break;

	case PF_TYPE_ANY:
	  break;	/* should not occur (but acceptable) */
	}
    }

  if (modifiable && fct_set == NULL)
    {
      switch(type)
	{
	case PF_TYPE_INTEGER:
	  flag->fct_set = Fct_Set_Integer;
	  break;

	case PF_TYPE_ATOM:
	  flag->fct_set = Fct_Set_Atom;
	  break;

	case PF_TYPE_ATOM_TBL:
	  flag->fct_set = Fct_Set_Atom_Tbl;
	  break;

	case PF_TYPE_ANY:
	  break;	/* should not occur */
	}
    }

  if (flag->type == PF_TYPE_ATOM_TBL)
    {
      va_list ap, ap2;
      int nb_elem, i;
      
      va_start(ap, fct_set);
      va_copy(ap2, ap);		/* for 2nd traversal or variadic args */
      nb_elem = 0;
      do
	nb_elem++;
      while(va_arg(ap, int) >= 0); /* count the -1 sentinel */
      va_end(ap);
      
      flag->tbl_atom = Calloc(nb_elem, sizeof(int));
      for(i = 0; i < nb_elem; i++)
	flag->tbl_atom[i] = va_arg(ap2, int);
      va_end(ap2);
    }

  return flag;
}




/*-------------------------------------------------------------------------*
 * PL_PROLOG_FLAG_LOOKUP                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static FlagInf *
Prolog_Flag_Lookup(int atom_name)
{
  int i;
  for(i = 0; i < nb_flag; i++)
    {
      if (flag_tbl[i].atom_name == atom_name)
	return flag_tbl + i;
    }

  return NULL;
}




/*-------------------------------------------------------------------------*
 * PL_SET_PROLOG_FLAG_2                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Set_Prolog_Flag_2(WamWord flag_word, WamWord value_word)
{
  WamWord word, tag_mask;
  int atom_name;
  FlagInf *flag;

  atom_name = Pl_Rd_Atom_Check(flag_word);

  flag = Prolog_Flag_Lookup(atom_name);

  if (flag == NULL)
    Pl_Err_Domain(pl_domain_prolog_flag, flag_word);

  DEREF(value_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();
  value_word = word; 		/* dereferenced */


  if (flag->fct_chk != NULL && !(*flag->fct_chk)(flag, tag_mask, value_word))
    {
      word = Pl_Put_Structure(ATOM_CHAR('+'), 2);
      Pl_Unify_Value(flag_word);
      Pl_Unify_Value(value_word);

      Pl_Err_Domain(pl_domain_flag_value, word);
    }
  
  if (!flag->modifiable)
    {
      Pl_Err_Permission(pl_permission_operation_modify, pl_permission_type_flag, flag_word);
    }

  return (*flag->fct_set)(flag, value_word);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_PROLOG_FLAG_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Prolog_Flag_2(WamWord flag_word, WamWord value_word)
{
  WamWord word, tag_mask;
  int atom_name;
  FlagInf *flag;
  int i;

  DEREF(flag_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    {
      atom_name = Pl_Rd_Atom_Check(word);

      flag = Prolog_Flag_Lookup(atom_name);

      if (flag == NULL)
	Pl_Err_Domain(pl_domain_prolog_flag, flag_word);

      return Pl_Unify((*flag->fct_get)(flag), value_word);
    }

  /* non deterministic case */
  i = 0;

  A(0) = flag_word;
  A(1) = value_word;
  A(2) = i + 1;
  Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_PROLOG_FLAG_ALT, 0), 3);

  flag = flag_tbl + i;
  Pl_Get_Atom(flag->atom_name, flag_word);

  return Pl_Unify((*flag->fct_get)(flag), value_word);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_PROLOG_FLAG_ALT_0                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Prolog_Flag_Alt_0(void)
{
  WamWord flag_word, value_word;
  FlagInf *flag;
  int i;

  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_PROLOG_FLAG_ALT, 0), 0);

  flag_word = AB(B, 0);
  value_word = AB(B, 1);
  i = (int) AB(B, 2);
  flag = flag_tbl + i;

  if (i + 1 == nb_flag)
    Delete_Last_Choice_Point();
  else				/* non deterministic case */
    {
#if 0 /* the following data is unchanged */
      AB(B, 0) = flag_word;
      AB(B, 1) = value_word;
#endif
      AB(B, 2) = i + 1;
    }

  Pl_Get_Atom(flag->atom_name, flag_word);

  return Pl_Unify((*flag->fct_get)(flag), value_word);
}
