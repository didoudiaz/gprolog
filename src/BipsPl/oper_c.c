/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : oper_c.c                                                        *
 * Descr.: operator management - C part                                    *
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

#define OBJ_INIT Oper_Initializer

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define FX                         0
#define FY                         1

#define XF                         2
#define YF                         3

#define XFX                        4
#define XFY                        5
#define YFX                        6




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int atom_specif_tbl[7];




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int Detect_Oper_Specif(OperInf *oper);



#define CURRENT_OP_ALT             X1_2463757272656E745F6F705F616C74

Prolog_Prototype(CURRENT_OP_ALT, 0);




/*-------------------------------------------------------------------------*
 * OPER_INITIALIZER                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Oper_Initializer(void)
{
  char *a[7] = { "fx", "fy", "xf", "yf", "xfx", "xfy", "yfx" };
  int i;

  for (i = 0; i < 7; i++)
    atom_specif_tbl[i] = Pl_Create_Atom(a[i]);
}




/*-------------------------------------------------------------------------*
 * PL_OP_3                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Op_3(WamWord prec_word, WamWord specif_word, WamWord oper_word)
{
  int atom_op;
  int prec;
  int atom_specif;
  int i;
  int type, left, right;


  atom_op = Pl_Rd_Atom_Check(oper_word);
  prec = Pl_Rd_Integer_Check(prec_word);
  if (prec < 0 || prec > MAX_PREC)
    Pl_Err_Domain(pl_domain_operator_priority, prec_word);

  atom_specif = Pl_Rd_Atom_Check(specif_word);

  for (i = 0; i < 7 && atom_specif != atom_specif_tbl[i]; i++)
    ;

  switch (i)
    {
    case FX:
    case FY:
      type = PREFIX;
      left = 0;
      right = (i == FX) ? prec - 1 : prec;
      break;

    case XF:
    case YF:
      type = POSTFIX;
      left = (i == XF) ? prec - 1 : prec;
      right = 0;
      break;

    case XFX:
    case XFY:
    case YFX:
      type = INFIX;
      left = (i == XFX || i == XFY) ? prec - 1 : prec;
      right = (i == XFX || i == YFX) ? prec - 1 : prec;
      break;

    default:
      Pl_Err_Domain(pl_domain_operator_specifier, specif_word);
      type = left = right = 0;	/* only for the compiler */
    }

  if ((type != PREFIX && Check_Oper(atom_op, (type == POSTFIX) ? INFIX : POSTFIX)) || /* infix + postfix invalid */
      (atom_op == ATOM_CHAR('|') && (type != INFIX || (prec > 0 && prec <= 1000))) || /* | no infix or prec <= 1000 */
      (atom_op == ATOM_NIL || atom_op == pl_atom_curly_brackets)) /* [] or {} forbidden */
    Pl_Err_Permission(pl_permission_operation_create,
		      pl_permission_type_operator,
		      oper_word);


  if (atom_op == ATOM_CHAR(','))
    Pl_Err_Permission(pl_permission_operation_modify,
		      pl_permission_type_operator,
		      oper_word);

  if (prec > 0)
    Pl_Create_Oper(atom_op, type, prec, left, right);
  else
    Pl_Delete_Oper(atom_op, type);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_OP_3                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Op_3(WamWord prec_word, WamWord specif_word, WamWord oper_word)
{
  WamWord word, tag_mask;
  HashScan scan;
  PlLong prec;
  int atom_specif;
  OperInf *oper;
  int atom;
  int op_mask;
  int i;

  DEREF(oper_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK)
    Pl_Err_Type(pl_type_atom, word);
  oper_word = word;


  DEREF(prec_word, word, tag_mask);
  prec = UnTag_INT(word);
  if (tag_mask != TAG_REF_MASK &&
      (tag_mask != TAG_INT_MASK || prec < 0 || prec > MAX_PREC))
    Pl_Err_Domain(pl_domain_operator_priority, word);
  prec_word = word;


  DEREF(specif_word, word, tag_mask);
  if (tag_mask == TAG_ATM_MASK)
    {
      atom_specif = UnTag_ATM(word);

      for (i = 0; i < 7 && atom_specif != atom_specif_tbl[i]; i++)
	;
    }

  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_ATM_MASK || i >= 7))
    Pl_Err_Domain(pl_domain_operator_specifier, specif_word);
  specif_word = word;


  if (Tag_Mask_Of(oper_word) == TAG_ATM_MASK)
    {
      atom = UnTag_ATM(oper_word);
      op_mask = pl_atom_tbl[atom].prop.op_mask;
      if (op_mask == 0)
	return FALSE;

      for (i = PREFIX; i <= POSTFIX; i++)
	if (op_mask & Make_Op_Mask(i))
	  break;

      op_mask &= ~Make_Op_Mask(i);
      if (op_mask)		/* non deterministic case */
	{
	  A(0) = prec_word;
	  A(1) = specif_word;
	  A(2) = oper_word;
	  A(3) = op_mask;
	  Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_OP_ALT, 0),
			      4);
	}
      oper = Pl_Lookup_Oper(atom, i);
    }
  else
    {
      oper = (OperInf *) Pl_Hash_First(pl_oper_tbl, &scan);
      if (oper == NULL)
	return FALSE;

      /* non deterministic case */
      A(0) = prec_word;
      A(1) = specif_word;
      A(2) = oper_word;
      A(3) = (WamWord) scan.endt;
      A(4) = (WamWord) scan.cur_t;
      A(5) = (WamWord) scan.cur_p;
      Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_OP_ALT, 0), 6);
    }

  return Pl_Get_Integer(oper->prec, prec_word) &&
    Pl_Get_Atom(Detect_Oper_Specif(oper), specif_word) &&
    Pl_Get_Atom(Atom_Of_Oper(oper->a_t), oper_word);
}




/*-------------------------------------------------------------------------*
 * PL_CURRENT_OP_ALT_0                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Current_Op_Alt_0(void)
{
  WamWord prec_word, specif_word, oper_word;
  HashScan scan;
  OperInf *oper;
  int atom;
  int op_mask;
  int i;


  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_OP_ALT, 0), 0);

  prec_word = AB(B, 0);
  specif_word = AB(B, 1);
  oper_word = AB(B, 2);

  if (Tag_Mask_Of(oper_word) == TAG_ATM_MASK)
    {
      atom = UnTag_ATM(oper_word);
      op_mask = AB(B, 3);

      for (i = PREFIX; i <= POSTFIX; i++)
	if (op_mask & Make_Op_Mask(i))
	  break;

      oper = Pl_Lookup_Oper(atom, i);

      Delete_Last_Choice_Point();
    }
  else
    {
      scan.endt = (char *) AB(B, 3);
      scan.cur_t = (char *) AB(B, 4);
      scan.cur_p = (char *) AB(B, 5);

      oper = (OperInf *) Pl_Hash_Next(&scan);
      if (oper == NULL)
	{
	  Delete_Last_Choice_Point();
	  return FALSE;
	}


#if 0 /* the following data is unchanged */
      AB(B, 0) = prec_word;
      AB(B, 1) = specif_word;
      AB(B, 2) = oper_word;
      AB(B, 3) = (WamWord) scan.endt;
#endif
      AB(B, 4) = (WamWord) scan.cur_t;
      AB(B, 5) = (WamWord) scan.cur_p;
    }

  return Pl_Get_Integer(oper->prec, prec_word) &&
    Pl_Get_Atom(Detect_Oper_Specif(oper), specif_word) &&
    Pl_Get_Atom(Atom_Of_Oper(oper->a_t), oper_word);
}




/*-------------------------------------------------------------------------*
 * DETECT_OPER_SPECIF                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Detect_Oper_Specif(OperInf *oper)
{
  int prec = oper->prec;
  int i;

  switch (Type_Of_Oper(oper->a_t))
    {
    case PREFIX:
      i = (oper->right) < prec ? FX : FY;
      break;

    case POSTFIX:
      i = (oper->left) < prec ? XF : YF;
      break;

    default:			/* i.e INFIX: */
      i = (oper->left) < prec ? ((oper->right) < prec ? XFX : XFY) : YFX;
      break;
    }

  return atom_specif_tbl[i];
}
