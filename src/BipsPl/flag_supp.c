/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : flag_supp.c                                                     *
 * Descr.: Prolog flag and system variable support                         *
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

#include "engine_pl.h"
#include "gprolog_cst.h"
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

static int atom_on;
static int atom_off;

/* pl_atom_error is already defined in stream_supp.[ch] */
static int atom_warning;
static int atom_fail;

static int atom_chars;
static int atom_codes;
static int atom_atom;
static int atom_chars_no_escape;
static int atom_codes_no_escape;
static int atom_atom_no_escape;

static int atom_toward_zero;
static int atom_down;




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
 * A flag has the following attributes (see flag_supp.h) :                 *
 *   - a name (string -> atom),                                            *
 *   - is modifiable or not (ro),                                          *
 *   - a type (several predefined types + 'any' for customization)         *
 *   - a value (a PlLong) used for predefined types and available for 'any'*
 *   - a function get: used by current_prolog_flag                         *
 *   - a function chk: used by set_prolog_flag (even if not modifiable)    *
 *   - a function set: used by set_prolog_flag (only if modifiable)        *
 *                                                                         *
 * The function chk is used by set_prolog_flag to check if the value is    *
 * compatible with the flag even if the flag is read-only. If the function *
 * fails a domain_error is raised (if it succeeds and the flag is read-only*
 * the a permission_error will be raised).                                 *
 *                                                                         *
 * For some flags it is not clear what to test. E.g. for version_data we   *
 * currently check the value is a struct gprolog/4 (args are not checked). *
 * This function can be omitted (in that case no domain_error is checked). *
 *                                                                         *
 * NB: this function is not used by current_prolog_flag (see ISO).         *
 *-------------------------------------------------------------------------*/


/*-------------------------------------------------------------------------*
 * INIT_FLAG_SUPP                                                          *
 *                                                                         *
 * no declared as other initializers, since we must be sure it has been    *
 * initialized before others (in particular before flag_c.c initializer).  *
 *-------------------------------------------------------------------------*/
static void
Init_Flag_Supp(void)
{
  static Bool initialized = FALSE;

  if (initialized)
    return;

  initialized = TRUE;

  atom_toward_zero = Pl_Create_Atom("toward_zero");
  atom_down = Pl_Create_Atom("down");

  atom_on = Pl_Create_Atom("on");
  atom_off = Pl_Create_Atom("off");

  atom_warning = Pl_Create_Atom("warning");
  atom_fail = Pl_Create_Atom("fail");

  atom_chars = Pl_Create_Atom("chars");
  atom_codes = Pl_Create_Atom("codes");
  atom_atom = Pl_Create_Atom("atom");
  atom_chars_no_escape = Pl_Create_Atom("chars_no_escape");
  atom_codes_no_escape = Pl_Create_Atom("codes_no_escape");
  atom_atom_no_escape = Pl_Create_Atom("atom_no_escape");
}




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
Fct_Get_Round(FlagInf *flag)
{
  return flag->value == PF_ROUND_ZERO ? Tag_ATM(atom_toward_zero) : Tag_ATM(atom_down);
}

static Bool
Fct_Chk_Round(FlagInf *flag, WamWord tag_mask, WamWord value_word)
{
  int atom = UnTag_ATM(value_word);

  return tag_mask == TAG_ATM_MASK && 
    (atom == atom_toward_zero || atom == atom_down);
}

static Bool
Fct_Set_Round(FlagInf *flag, WamWord value_word)
{
  int atom = UnTag_ATM(value_word);
  flag->value = (atom == atom_toward_zero) ? PF_ROUND_ZERO : PF_ROUND_DOWN;
  return TRUE;
}




static WamWord
Fct_Get_Bool(FlagInf *flag)
{
  return flag->value ? Tag_ATM(pl_atom_true) : Tag_ATM(pl_atom_false);
}

static Bool
Fct_Chk_Bool(FlagInf *flag, WamWord tag_mask, WamWord value_word)
{
  int atom = UnTag_ATM(value_word);

  return tag_mask == TAG_ATM_MASK &&
    (atom == pl_atom_true || atom == pl_atom_false);
}

static Bool
Fct_Set_Bool(FlagInf *flag, WamWord value_word)
{
  int atom = UnTag_ATM(value_word);
  flag->value = (atom == pl_atom_true);
  return TRUE;
}




static WamWord
Fct_Get_On_Off(FlagInf *flag)
{
  return flag->value ? Tag_ATM(atom_on) : Tag_ATM(atom_off);
}

static Bool
Fct_Chk_On_Off(FlagInf *flag, WamWord tag_mask, WamWord value_word)
{
  int atom = UnTag_ATM(value_word);

  return tag_mask == TAG_ATM_MASK &&
    (atom == atom_on || atom == atom_off);
}

static Bool
Fct_Set_On_Off(FlagInf *flag, WamWord value_word)
{
  int atom = UnTag_ATM(value_word);
  flag->value = (atom == atom_on);
  return TRUE;
}




static WamWord
Fct_Get_Err(FlagInf *flag)
{
  int atom;

  switch (flag->value)
    {
    case PF_ERR_ERROR:
      atom = pl_atom_error;
      break;

    case PF_ERR_WARNING:
      atom = atom_warning;
      break;

    case PF_ERR_FAIL:
      atom = atom_fail;
      break;
    }

  return Tag_ATM(atom);
}

static Bool
Fct_Chk_Err(FlagInf *flag, WamWord tag_mask, WamWord value_word)
{
  int atom = UnTag_ATM(value_word);

  return tag_mask == TAG_ATM_MASK && 
    (atom == pl_atom_error || atom == atom_warning || atom == atom_fail);
}

static Bool
Fct_Set_Err(FlagInf *flag, WamWord value_word)
{
  int atom = UnTag_ATM(value_word);

  if (atom == pl_atom_error)
    flag->value = PF_ERR_ERROR;
  else if (atom == atom_warning)
    flag->value = PF_ERR_WARNING;
  else
    flag->value = PF_ERR_FAIL;

  return TRUE;
}




static WamWord
Fct_Get_Quotes(FlagInf *flag)
{
  int atom;

  switch (flag->value)
    {
    case PF_QUOT_AS_CODES:
      atom = atom_codes;
      break;

    case PF_QUOT_AS_CODES | PF_QUOT_NO_ESCAPE_MASK:
      atom = atom_codes_no_escape;
      break;

    case PF_QUOT_AS_CHARS:
      atom = atom_chars;
      break;

    case PF_QUOT_AS_CHARS | PF_QUOT_NO_ESCAPE_MASK:
      atom = atom_chars_no_escape;
      break;

    case PF_QUOT_AS_ATOM:
      atom = atom_atom;
      break;

    case PF_QUOT_AS_ATOM | PF_QUOT_NO_ESCAPE_MASK:
      atom = atom_atom_no_escape;
      break;
    }

  return Tag_ATM(atom);
}

static Bool
Fct_Chk_Quotes(FlagInf *flag, WamWord tag_mask, WamWord value_word)
{
  int atom = UnTag_ATM(value_word);

  return tag_mask == TAG_ATM_MASK && 
    (atom == atom_codes || atom == atom_codes_no_escape ||
     atom == atom_chars || atom == atom_chars_no_escape ||
     atom == atom_atom  || atom == atom_atom_no_escape);
}

static Bool
Fct_Set_Quotes(FlagInf *flag, WamWord value_word)
{
  int atom = UnTag_ATM(value_word);

  if (atom == atom_codes)
    flag->value = PF_QUOT_AS_CODES;
  else if (atom == atom_codes_no_escape)
    flag->value = PF_QUOT_AS_CODES | PF_QUOT_NO_ESCAPE_MASK;
  else if (atom == atom_chars)
    flag->value = PF_QUOT_AS_CHARS;
  else if (atom == atom_chars_no_escape)
    flag->value = PF_QUOT_AS_CHARS | PF_QUOT_NO_ESCAPE_MASK;
  else if (atom == atom_atom)
    flag->value = PF_QUOT_AS_ATOM;
  else
    flag->value = PF_QUOT_AS_ATOM | PF_QUOT_NO_ESCAPE_MASK;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_NEW_PROLOG_FLAG                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
FlagInf *
Pl_New_Prolog_Flag(char *name, Bool modifiable, FlagType type, PlLong value,
		   FlagFctGet fct_get, FlagFctChk fct_chk, FlagFctSet fct_set)
{
  int atom_name;
  FlagInf *flag;

  Init_Flag_Supp();

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

	case PF_TYPE_BOOL: 
	  flag->fct_get = Fct_Get_Bool; 
	  break;

	case PF_TYPE_ON_OFF: 
	  flag->fct_get = Fct_Get_On_Off;
	  break;

	case PF_TYPE_ERR: 
	  flag->fct_get = Fct_Get_Err;
	  break;

	case PF_TYPE_QUOTES: 
	  flag->fct_get = Fct_Get_Quotes;
	  break;

	case PF_TYPE_ROUND: 
	  flag->fct_get = Fct_Get_Round;
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

	case PF_TYPE_BOOL: 
	  flag->fct_chk = Fct_Chk_Bool; 
	  break;

	case PF_TYPE_ON_OFF: 
	  flag->fct_chk = Fct_Chk_On_Off;
	  break;

	case PF_TYPE_ERR: 
	  flag->fct_chk = Fct_Chk_Err;
	  break;

	case PF_TYPE_QUOTES: 
	  flag->fct_chk = Fct_Chk_Quotes;
	  break;

	case PF_TYPE_ROUND: 
	  flag->fct_chk = Fct_Chk_Round;
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

	case PF_TYPE_BOOL:
	  flag->fct_set = Fct_Set_Bool;
	  break;

	case PF_TYPE_ON_OFF:
	  flag->fct_set = Fct_Set_On_Off;
	  break;

	case PF_TYPE_ERR: 
	  flag->fct_set = Fct_Set_Err;
	  break;

	case PF_TYPE_QUOTES: 
	  flag->fct_set = Fct_Set_Quotes;
	  break;

	case PF_TYPE_ROUND:
	  flag->fct_set = Fct_Set_Round;
	  break;

	case PF_TYPE_ANY: 
	  break;	/* should not occur */
	}
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
  i = AB(B, 2);
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
