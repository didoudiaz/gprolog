/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : pred_c.c                                                        *
 * Descr.: predicate manipulation management - C part                      *
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

#define CURRENT_PREDICATE_ALT   X1_2463757272656E745F7072656469636174655F616C74

Prolog_Prototype(CURRENT_PREDICATE_ALT, 0);




/*-------------------------------------------------------------------------*
 * PL_CURRENT_PREDICATE_2                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Predicate_2(WamWord pred_indic_word, WamWord which_preds_word)
{
  WamWord name_word, arity_word;
  HashScan scan;
  PredInf *pred;
  int func, arity;
  WamWord module_word;
  int func1, arity1;
  int which_preds;		/* 0=user, 1=user+bips, 2=user+bips+system */
  Bool all;

  /* FIXME: use module */
  module_word = Pl_Get_Pred_Indicator(pred_indic_word, FALSE, &func, &arity);
  name_word = pl_pi_name_word;
  arity_word = pl_pi_arity_word;

  which_preds = Pl_Rd_Integer(which_preds_word);

  if (which_preds == 0 && !Flag_Value(FLAG_STRICT_ISO))
    which_preds = 1;

#define Pred_Is_Ok(pred, func, which_preds) \
  (which_preds == 2 || (pl_atom_tbl[func].name[0] != '$' && \
   (which_preds == 1 || !(pred->prop & MASK_PRED_BUILTIN))))

  if (func >= 0 && arity >= 0)
    {
      pred = Pl_Lookup_Pred_Compat(func, arity);
      return pred && Pred_Is_Ok(pred, func, which_preds);
    }

				/* here func or arity == -1 (or both) */
  all = (func == -1 && arity == -1);

  pred = (PredInf *) Pl_Hash_First(pl_pred_tbl, &scan);
  for (;;)
    {
      if (pred == NULL)
	return FALSE;

      func1 = Functor_Of(pred->f_n);
      arity1 = Arity_Of(pred->f_n);

      if ((all || func == func1 || arity == arity1) &&
	  Pred_Is_Ok(pred, func1, which_preds))
	break;

      pred = (PredInf *) Pl_Hash_Next(&scan);
    }

				/* non deterministic case */
  A(0) = name_word;
  A(1) = arity_word;
  A(2) = which_preds;
  A(3) = (WamWord) scan.endt;
  A(4) = (WamWord) scan.cur_t;
  A(5) = (WamWord) scan.cur_p;
  Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_PREDICATE_ALT, 0), 6);

  return Pl_Get_Atom(Functor_Of(pred->f_n), name_word) &&
    Pl_Get_Integer(Arity_Of(pred->f_n), arity_word);
  /*
  return Pl_Un_Atom_Check(Functor_Of(pred->f_n), name_word) &&
    Pl_Un_Integer_Check(Arity_Of(pred->f_n), arity_word);
  */
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_PREDICATE_ALT_0                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Predicate_Alt_0(void)
{
  WamWord name_word, arity_word;
  HashScan scan;
  PredInf *pred;
  int which_preds;
  int func, arity;
  int func1, arity1;
  Bool all;

  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_PREDICATE_ALT, 0),
		      0);

  name_word = AB(B, 0);
  arity_word = AB(B, 1);
  which_preds = AB(B, 2);
  scan.endt = (char *) AB(B, 3);
  scan.cur_t = (char *) AB(B, 4);
  scan.cur_p = (char *) AB(B, 5);

  func = Tag_Mask_Of(name_word) == TAG_REF_MASK ? -1 : UnTag_ATM(name_word);
  arity = Tag_Mask_Of(arity_word) == TAG_REF_MASK ? -1 : UnTag_INT(arity_word);

				/* here func or arity == -1 (or both) */
  all = (func == -1 && arity == -1);

  for (;;)
    {
      pred = (PredInf *) Pl_Hash_Next(&scan);
      if (pred == NULL)
	{
	  Delete_Last_Choice_Point();
	  return FALSE;
	}

      func1 = Functor_Of(pred->f_n);
      arity1 = Arity_Of(pred->f_n);

      if ((all || func == func1 || arity == arity1) &&
	  Pred_Is_Ok(pred, func1, which_preds))
	break;
    }

				/* non deterministic case */

#if 0				/* the following data is unchanged */
  AB(B, 0) = name_word;
  AB(B, 1) = arity_word;
  AB(B, 2) = which_preds;
  AB(B, 3) = (WamWord) scan.endt;
#endif
  AB(B, 4) = (WamWord) scan.cur_t;
  AB(B, 5) = (WamWord) scan.cur_p;

  return Pl_Get_Atom(Functor_Of(pred->f_n), name_word) &&
    Pl_Get_Integer(Arity_Of(pred->f_n), arity_word);
}





/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_STATIC_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Static_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && (pred->prop & MASK_PRED_DYNAMIC) == 0;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_DYNAMIC_2                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Dynamic_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && (pred->prop & MASK_PRED_DYNAMIC) != 0;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_PRIVATE_2                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Private_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && (pred->prop & MASK_PRED_PUBLIC) == 0;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_PUBLIC_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Public_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && (pred->prop & MASK_PRED_PUBLIC) != 0;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_MONOFILE_2                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Monofile_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && (pred->prop & MASK_PRED_MULTIFILE) == 0;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_MULTIFILE_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Multifile_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && (pred->prop & MASK_PRED_MULTIFILE) != 0;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_USER_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_User_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && (pred->prop & MASK_PRED_BUILTIN) == 0;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_BUILT_IN_2                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Built_In_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && (pred->prop & MASK_PRED_BUILTIN) != 0;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_BUILT_IN_FD_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Built_In_Fd_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && (pred->prop & MASK_PRED_BUILTIN_FD) != 0;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_CONTROL_CONSTRUCT_2                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Control_Construct_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && (pred->prop & MASK_PRED_CONTROL_CONSTRUCT) != 0;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_NATIVE_CODE_2                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Native_Code_2(WamWord func_word, WamWord arity_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && (pred->prop & MASK_PRED_NATIVE_CODE) != 0;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_META_PREDICATE_3                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Meta_Predicate_3(WamWord func_word, WamWord arity_word, 
			      WamWord meta_pred_spec_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);
  MetaSpec meta_spec;
  WamWord word;
  int i, x;

  if (pred == NULL || !(pred->prop & MASK_PRED_META_PRED))
    return FALSE;

  if (!Pl_Get_Structure(func, arity, meta_pred_spec_word))
    return FALSE;

  meta_spec = pred->meta_spec;

  for(i = 0; i < arity; i++)
    {
      x = meta_spec & 0xf;
      meta_spec >>= 4;

      switch(x)
	{
	case META_PRED_ARG_COLON:
	  word = Tag_ATM(ATOM_CHAR(':')); 
	  break;

	case META_PRED_ARG_PLUS:
	  word = Tag_ATM(ATOM_CHAR('+')); 
	  break;

	case META_PRED_ARG_MINUS:
	  word = Tag_ATM(ATOM_CHAR('-')); 
	  break;

	case META_PRED_ARG_QUESTION:
	  word = Tag_ATM(ATOM_CHAR('?')); 
	  break;

	default: 		/* an integer */
	  word = Tag_INT(x);
	}
      if (!Pl_Unify_Value(word))
	return FALSE;
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_PROLOG_FILE_3                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Prolog_File_3(WamWord func_word, WamWord arity_word, WamWord pl_file_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && Pl_Un_Atom_Check(pred->pl_file, pl_file_word);
}




/*-------------------------------------------------------------------------*
 * PL_PRED_PROP_PROLOG_LINE_3                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Prop_Prolog_Line_3(WamWord func_word, WamWord arity_word, WamWord pl_line_word)
{
  int func = Pl_Rd_Atom(func_word);
  int arity = Pl_Rd_Integer(arity_word);
  PredInf *pred = Pl_Lookup_Pred_Compat(func, arity);

  return pred != NULL && Pl_Un_Integer_Check(pred->pl_line, pl_line_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_PREDICATE_FILE_INFO_3                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Predicate_File_Info_3(WamWord pred_indic_word,
			     WamWord pl_file_word, WamWord pl_line_word)
{
  int module, func, arity;
  PredInf *pred;

  /* FIXME: use module */
  module = Pl_Get_Pred_Indicator(pred_indic_word, FALSE, &func, &arity);

  if ((pred = Pl_Lookup_Pred_Compat(func, arity)) == NULL)
    return FALSE;

  if (pred->pl_file == pl_atom_void || pred->pl_line == 0)
    return FALSE;

  return Pl_Un_Atom_Check(pred->pl_file, pl_file_word) &&
    Pl_Un_Integer_Check(pred->pl_line, pl_line_word);
}




/*-------------------------------------------------------------------------*
 * PL_SET_PREDICATE_FILE_INFO_3                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Set_Predicate_File_Info_3(WamWord pred_indic_word,
			     WamWord pl_file_word, WamWord pl_line_word)
{
  int module, func, arity;
  int pl_file, pl_line;
  PredInf *pred;

  /* FIXME: use module */
  module = Pl_Get_Pred_Indicator(pred_indic_word, FALSE, &func, &arity);

  if ((pred = Pl_Lookup_Pred_Compat(func, arity)) == NULL)
    return FALSE;

  pl_file = Pl_Rd_Atom_Check(pl_file_word);
  pl_line = Pl_Rd_Integer_Check(pl_line_word);

  if (pl_line < 0)
    return FALSE;

  pred->pl_file = pl_file;
  pred->pl_line = pl_line;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_AUX_NAME_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Aux_Name_1(WamWord name_word)
{
  int func;

  func = Pl_Rd_Atom_Check(name_word);
  return Pl_Detect_If_Aux_Name(func) != NULL;
}




/*-------------------------------------------------------------------------*
 * PL_FATHER_OF_AUX_NAME_3                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Father_Of_Aux_Name_3(WamWord name_word, WamWord father_name_word,
			WamWord father_arity_word)
{
  int func, father_func, father_arity;

  func = Pl_Rd_Atom_Check(name_word);
  father_func = Pl_Father_Pred_Of_Aux(func, &father_arity);

  if (father_func < 0)
    return FALSE;

  return Pl_Un_Atom_Check(father_func, father_name_word) &&
    Pl_Un_Integer_Check(father_arity, father_arity_word);
}




/*-------------------------------------------------------------------------*
 * PL_PRED_WITHOUT_AUX_4                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Pred_Without_Aux_4(WamWord name_word, WamWord arity_word,
		      WamWord name1_word, WamWord arity1_word)
{
  int func, arity;
  int func1, arity1;

  func = Pl_Rd_Atom_Check(name_word);
  arity = Pl_Rd_Integer_Check(arity_word);

  func1 = Pl_Pred_Without_Aux(func, arity, &arity1);

  return Pl_Un_Atom_Check(func1, name1_word) &&
    Pl_Un_Integer_Check(arity1, arity1_word);
}






/*-------------------------------------------------------------------------*
 * PL_MAKE_AUX_NAME_4                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Make_Aux_Name_4(WamWord name_word, WamWord arity_word,
		   WamWord aux_nb_word, WamWord aux_name_word)
{
  int func, arity;
  int aux_nb;
  int aux_name;

  func = Pl_Rd_Atom_Check(name_word);
  arity = Pl_Rd_Integer_Check(arity_word);
  aux_nb = Pl_Rd_Integer_Check(aux_nb_word);

  aux_name = Pl_Make_Aux_Name(func, arity, aux_nb);

  return Pl_Un_Atom_Check(aux_name, aux_name_word);
}




/*-------------------------------------------------------------------------*
 * PL_DECL_META_PRED_2                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Decl_Meta_Pred_2(WamWord module_word, WamWord meta_pred_spec_word)
{
  int module;
  int func, arity;
  WamWord *arg_adr;
  PredInf *pred;
  WamWord word, tag_mask;
  int i, x;
  int atom;
  MetaSpec meta_spec = 0;

  module = Pl_Rd_Atom_Check(module_word);
  
  arg_adr = Pl_Rd_Compound_Check(meta_pred_spec_word, &func, &arity);

  if (arity > 16)		/* 4-bits/arg on 64 bits = 16 agrs max */
    return FALSE;		/* FIXME: emit a representation error max_arity_meta_predicate  */

  pred = Pl_Lookup_Pred(module, func, arity);
  if (pred == NULL)
    return FALSE;

  for (i = 0; i < arity; i++, arg_adr++)
    {
      DEREF(*arg_adr, word, tag_mask);
      if (tag_mask == TAG_REF_MASK) /* should not occur */
	Pl_Err_Instantiation();
      if (tag_mask == TAG_INT_MASK)
	{
	  x = UnTag_INT(word);
	  if (x >= 0 && x < META_PRED_ARG_COLON)
	    {
	    ok:
	      meta_spec |= x << (i * 4);
	      continue;
	    }
	}
      if (tag_mask == TAG_ATM_MASK)
	{
	  x = -1;
	  atom = UnTag_ATM(word);
	  if (atom == ATOM_CHAR(':'))
	    x = META_PRED_ARG_COLON;
	  else if (atom == ATOM_CHAR('+'))
	    x = META_PRED_ARG_PLUS;
	  else if (atom == ATOM_CHAR('-'))
	    x = META_PRED_ARG_MINUS;
	  else if (atom == ATOM_CHAR('?') || atom == ATOM_CHAR('*'))
	    x = META_PRED_ARG_QUESTION;
	  if (x >= 0)
	    goto ok;
	}

#if 0 				/* FIXME: should be a domain error meta_argument_specifier */
      Pl_Err_Domain(blabla, word);
#endif
    }

  pred->prop |= MASK_PRED_META_PRED;

  pred->meta_spec = meta_spec;

  return TRUE;
}





/*-------------------------------------------------------------------------*
 * PL_STRIP_MODULE_3                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Strip_Module_3(WamWord term_word, WamWord module_word, WamWord plain_word)
{
  WamWord goal_word, module_word1;
  
  module_word1 = Pl_Strip_Module(term_word, FALSE, FALSE, &goal_word);

#if 1				/* should not occur since meta_predicate declaration */
  if (module_word == NOT_A_WAM_WORD)
    module_word = Tag_ATM(pl_atom_user);
#endif

  return Pl_Unify(module_word1, module_word) && Pl_Unify(goal_word, plain_word);
}
