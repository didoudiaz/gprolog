/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : pred_c.c                                                        *
 * Descr.: predicate manipulation management - C part                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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

#include <stdlib.h>
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

#define CURRENT_PREDICATE_ALT   X2463757272656E745F7072656469636174655F616C74

Prolog_Prototype(CURRENT_PREDICATE_ALT, 0)




/*-------------------------------------------------------------------------*
 * CURRENT_PREDICATE_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
     Bool Current_Predicate_2(WamWord pred_indic_word, WamWord hide_word)
{
  WamWord word, tag, *adr;
  WamWord name_word, arity_word;
  HashScan scan;
  PredInf *pred;
  Bool hide;
  int func, arity;
  int func1, arity1;
  Bool all;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);
  name_word = pi_name_word;
  arity_word = pi_arity_word;

  Deref(hide_word, word, tag, adr);
  hide = UnTag_INT(word);


  if (func >= 0)
    {
      if (hide && atom_tbl[func].name[0] == '$')
	return FALSE;

      if (arity >= 0)
	return Lookup_Pred(func, arity) != NULL;
    }

  all = (arity == -1);

  pred = (PredInf *) Hash_First(pred_tbl, &scan);
  for (;;)
    {
      if (pred == NULL)
	return FALSE;

      if (!hide || !Flag_Value(FLAG_STRICT_ISO) ||
	  !(pred->prop & MASK_PRED_ANY_BUILTIN))
	{
	  func1 = Functor_Of(pred->f_n);
	  arity1 = Arity_Of(pred->f_n);

	  if ((all || func == func1 || arity == arity1) &&
	      (!hide || atom_tbl[func1].name[0] != '$'))
	    break;
	}

      pred = (PredInf *) Hash_Next(&scan);
    }

  /* non deterministic case */
  A(0) = name_word;
  A(1) = arity_word;
  A(2) = hide;
  A(3) = (WamWord) scan.endt;
  A(4) = (WamWord) scan.cur_t;
  A(5) = (WamWord) scan.cur_p;
  Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_PREDICATE_ALT, 0),
		      6);

  return Un_Atom_Check(Functor_Of(pred->f_n), name_word) &&
    Un_Integer_Check(Arity_Of(pred->f_n), arity_word);
}




/*-------------------------------------------------------------------------*
 * CURRENT_PREDICATE_ALT_0                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Current_Predicate_Alt_0(void)
{
  WamWord name_word, arity_word;
  HashScan scan;
  PredInf *pred;
  Bool hide;
  int func, arity;
  int func1, arity1;
  Bool all;

  Update_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_PREDICATE_ALT, 0),
		      0);

  name_word = AB(B, 0);
  arity_word = AB(B, 1);
  hide = AB(B, 2);
  scan.endt = (char *) AB(B, 3);
  scan.cur_t = (char *) AB(B, 4);
  scan.cur_p = (char *) AB(B, 5);

  func = Tag_Of(name_word) == REF ? -1 : UnTag_ATM(name_word);
  arity = Tag_Of(arity_word) == REF ? -1 : UnTag_INT(arity_word);
  if (func >= 0)
    {
      if (hide && atom_tbl[func].name[0] == '$')
	return FALSE;

      if (arity >= 0)
	return Lookup_Pred(func, arity) != NULL;
    }
  all = (arity == -1);

  for (;;)
    {
      pred = (PredInf *) Hash_Next(&scan);
      if (pred == NULL)
	{
	  Delete_Last_Choice_Point();
	  return FALSE;
	}

      if (!hide || !Flag_Value(FLAG_STRICT_ISO) ||
	  !(pred->prop & MASK_PRED_ANY_BUILTIN))
	{
	  func1 = Functor_Of(pred->f_n);
	  arity1 = Arity_Of(pred->f_n);

	  if ((all || func == func1 || arity == arity1) &&
	      (!hide || atom_tbl[func1].name[0] != '$'))
	    break;
	}
    }
  /* non deterministic case */

#if 0 /* the following data is unchanged */
  AB(B, 0) = name_word;
  AB(B, 1) = arity_word;
  AB(B, 2) = hide;
  AB(B, 3) = (WamWord) scan.endt;
#endif
  AB(B, 4) = (WamWord) scan.cur_t;
  AB(B, 5) = (WamWord) scan.cur_p;

  return Get_Atom(Functor_Of(pred->f_n), name_word) &&
    Get_Integer(Arity_Of(pred->f_n), arity_word);
}




/*-------------------------------------------------------------------------*
 * PRED_PROP_STATIC_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pred_Prop_Static_1(WamWord pred_indic_word)
{
  int func, arity;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  return (pred->prop & MASK_PRED_DYNAMIC) == 0;
}




/*-------------------------------------------------------------------------*
 * PRED_PROP_DYNAMIC_1                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pred_Prop_Dynamic_1(WamWord pred_indic_word)
{
  int func, arity;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  return (pred->prop & MASK_PRED_DYNAMIC) != 0;
}




/*-------------------------------------------------------------------------*
 * PRED_PROP_PRIVATE_1                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pred_Prop_Private_1(WamWord pred_indic_word)
{
  int func, arity;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  return (pred->prop & MASK_PRED_PUBLIC) == 0;
}




/*-------------------------------------------------------------------------*
 * PRED_PROP_PUBLIC_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pred_Prop_Public_1(WamWord pred_indic_word)
{
  int func, arity;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  return (pred->prop & MASK_PRED_PUBLIC) != 0;
}




/*-------------------------------------------------------------------------*
 * PRED_PROP_USER_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pred_Prop_User_1(WamWord pred_indic_word)
{
  int func, arity;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  return (pred->prop & MASK_PRED_ANY_BUILTIN) == 0;
}




/*-------------------------------------------------------------------------*
 * PRED_PROP_BUILT_IN_1                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pred_Prop_Built_In_1(WamWord pred_indic_word)
{
  int func, arity;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  return (pred->prop & MASK_PRED_BUILTIN) != 0;
}




/*-------------------------------------------------------------------------*
 * PRED_PROP_BUILT_IN_FD_1                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pred_Prop_Built_In_Fd_1(WamWord pred_indic_word)
{
  int func, arity;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  return (pred->prop & MASK_PRED_BUILTIN_FD) != 0;
}




/*-------------------------------------------------------------------------*
 * PRED_PROP_NATIVE_CODE_1                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pred_Prop_Native_Code_1(WamWord pred_indic_word)
{
  int func, arity;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  return (pred->prop & MASK_PRED_NATIVE_CODE) != 0;
}




/*-------------------------------------------------------------------------*
 * PRED_PROP_PROLOG_FILE_2                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pred_Prop_Prolog_File_2(WamWord pred_indic_word, WamWord pl_file_word)
{
  int func, arity;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  return Un_Atom_Check(pred->pl_file, pl_file_word);
}




/*-------------------------------------------------------------------------*
 * PRED_PROP_PROLOG_LINE_2                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pred_Prop_Prolog_Line_2(WamWord pred_indic_word, WamWord pl_line_word)
{
  int func, arity;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  return Un_Integer_Check(pred->pl_line, pl_line_word);
}




/*-------------------------------------------------------------------------*
 * GET_PREDICATE_FILE_INFO_3                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Predicate_File_Info_3(WamWord pred_indic_word,
			  WamWord pl_file_word, WamWord pl_line_word)
{
  int func, arity;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  if (pred->pl_file == atom_void || pred->pl_line == 0)
    return FALSE;

  return Un_Atom_Check(pred->pl_file, pl_file_word) &&
    Un_Integer_Check(pred->pl_line, pl_line_word);
}




/*-------------------------------------------------------------------------*
 * SET_PREDICATE_FILE_INFO_3                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Set_Predicate_File_Info_3(WamWord pred_indic_word,
			  WamWord pl_file_word, WamWord pl_line_word)
{
  int func, arity;
  int pl_file, pl_line;
  PredInf *pred;

  func = Get_Pred_Indicator(pred_indic_word, FALSE, &arity);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  pl_file = Rd_Atom_Check(pl_file_word);
  pl_line = Rd_Integer_Check(pl_line_word);

  if (pl_line < 0)
    return FALSE;

  pred->pl_file = pl_file;
  pred->pl_line = pl_line;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * AUX_NAME_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Aux_Name_1(WamWord name_word)
{
  int func;

  func = Rd_Atom_Check(name_word);
  return Detect_If_Aux_Name(func) != NULL;
}




/*-------------------------------------------------------------------------*
 * FATHER_OF_AUX_NAME_3                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Father_Of_Aux_Name_3(WamWord name_word, WamWord father_name_word,
		     WamWord father_arity_word)
{
  int func, father_func, father_arity;

  func = Rd_Atom_Check(name_word);
  father_func = Father_Pred_Of_Aux(func, &father_arity);

  if (father_func < 0)
    return FALSE;

  return Un_Atom_Check(father_func, father_name_word) &&
    Un_Integer_Check(father_arity, father_arity_word);
}




/*-------------------------------------------------------------------------*
 * PRED_WITHOUT_AUX_4                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pred_Without_Aux_4(WamWord name_word, WamWord arity_word,
		   WamWord name1_word, WamWord arity1_word)
{
  int func, arity;
  int func1, arity1;

  func = Rd_Atom_Check(name_word);
  arity = Rd_Integer_Check(arity_word);

  func1 = Pred_Without_Aux(func, arity, &arity1);

  return Un_Atom_Check(func1, name1_word) &&
    Un_Integer_Check(arity1, arity1_word);
}






/*-------------------------------------------------------------------------*
 * MAKE_AUX_NAME_4                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Make_Aux_Name_4(WamWord name_word, WamWord arity_word,
		WamWord aux_nb_word, WamWord aux_name_word)
{
  int func, arity;
  int aux_nb;
  int aux_name;

  func = Rd_Atom_Check(name_word);
  arity = Rd_Integer_Check(arity_word);
  aux_nb = Rd_Integer_Check(aux_nb_word);

  aux_name = Make_Aux_Name(func, arity, aux_nb);

  return Un_Atom_Check(aux_name, aux_name_word);
}
